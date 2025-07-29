{-# LANGUAGE RecordWildCards #-}

module CategoricalAnholonomic.Metric where

import CategoricalAnholonomic.Core
import CategoricalAnholonomic.InfoCategory
import CategoricalAnholonomic.AnholonomicField
import Data.Complex
import qualified Data.Vector as V
import Numeric.LinearAlgebra hiding (Vector)
import Control.Monad.State

data MetricTensor = MetricTensor
    { metricComponents :: (Double, Double, Double) -> Matrix Double
    , metricInverse :: (Double, Double, Double) -> Matrix Double
    , metricDeterminant :: (Double, Double, Double) -> Double
    }

inducedMetric :: AnholonomicField -> MetricTensor
inducedMetric field = MetricTensor
    { metricComponents = computeMetric field
    , metricInverse = computeInverseMetric field
    , metricDeterminant = computeMetricDet field
    }
  where
    computeMetric :: AnholonomicField -> (Double, Double, Double) -> Matrix Double
    computeMetric field point = 
        let state = getFieldState field point
            basis = orthonormalBasis 3
            gij = (3><3) [innerProduct (evolve i) (evolve j) | i <- basis, j <- basis]
        in gij
      where
        evolve v = fieldEvolution field point v
        innerProduct s1 s2 = fidelity s1 s2
    
    computeInverseMetric :: AnholonomicField -> (Double, Double, Double) -> Matrix Double
    computeInverseMetric field point = inv (computeMetric field point)
    
    computeMetricDet :: AnholonomicField -> (Double, Double, Double) -> Double
    computeMetricDet field point = det (computeMetric field point)

getFieldState :: AnholonomicField -> (Double, Double, Double) -> InfoState
getFieldState field point = 
    let idx = V.minIndexBy (\p1 p2 -> compare (distance p1 point) (distance p2 point)) (fieldPoints field)
    in fieldValues field V.! idx
  where
    distance (x1,y1,z1) (x2,y2,z2) = sqrt ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

fieldEvolution :: AnholonomicField -> (Double, Double, Double) -> V.Vector Double -> InfoState
fieldEvolution field point direction = 
    let tangent = TangentVector point (direction V.! 0, direction V.! 1, direction V.! 2)
        path = generatePath point tangent 0.1
        loop = Loop point path True
    in parallelTransportField field loop (getFieldState field point)
  where
    generatePath base (TangentVector _ (vx,vy,vz)) dt = 
        let (x,y,z) = base
        in [(x + vx*t, y + vy*t, z + vz*t) | t <- [0, dt/10 .. dt]]

orthonormalBasis :: Int -> [V.Vector Double]
orthonormalBasis n = [V.generate n (\j -> if i == j then 1 else 0) | i <- [0..n-1]]

christoffelSymbols :: MetricTensor -> (Double, Double, Double) -> Array Double
christoffelSymbols metric point = 
    let g = metricComponents metric point
        ginv = metricInverse metric point
        dg = metricDerivatives metric point
    in computeChristoffel ginv dg
  where
    metricDerivatives :: MetricTensor -> (Double, Double, Double) -> [Matrix Double]
    metricDerivatives metric (x,y,z) = 
        let eps = 1e-6
            gx = scale (1/eps) $ metricComponents metric (x+eps,y,z) - metricComponents metric (x,y,z)
            gy = scale (1/eps) $ metricComponents metric (x,y+eps,z) - metricComponents metric (x,y,z)
            gz = scale (1/eps) $ metricComponents metric (x,y,z+eps) - metricComponents metric (x,y,z)
        in [gx, gy, gz]
    
    computeChristoffel :: Matrix Double -> [Matrix Double] -> Array Double
    computeChristoffel ginv dg = listArray ((0,0,0), (2,2,2)) 
        [0.5 * sum [ginv @@> (i,m) * (dg!!j @@> (m,k) + dg!!k @@> (m,j) - dg!!m @@> (j,k))
                   | m <- [0,1,2]]
        | i <- [0,1,2], j <- [0,1,2], k <- [0,1,2]]

riemannTensor :: MetricTensor -> (Double, Double, Double) -> Array Double
riemannTensor metric point = 
    let gamma = christoffelSymbols metric point
        eps = 1e-6
        dgamma = christoffelDerivatives metric point eps
    in listArray ((0,0,0,0), (2,2,2,2))
        [dgamma ! (i,j,l,k) - dgamma ! (i,k,l,j) 
         + sum [gamma ! (i,j,m) * gamma ! (m,k,l) - gamma ! (i,k,m) * gamma ! (m,j,l)
               | m <- [0,1,2]]
        | i <- [0,1,2], j <- [0,1,2], k <- [0,1,2], l <- [0,1,2]]
  where
    christoffelDerivatives metric point eps = 
        let gamma_plus = christoffelSymbols metric (fst3 point + eps, snd3 point, trd3 point)
            gamma_minus = christoffelSymbols metric (fst3 point - eps, snd3 point, trd3 point)
        in listArray ((0,0,0,0), (2,2,2,2)) 
            [(gamma_plus ! (i,j,k) - gamma_minus ! (i,j,k)) / (2 * eps)
            | i <- [0,1,2], j <- [0,1,2], k <- [0,1,2], _ <- [0,1,2]]
    
    fst3 (x,_,_) = x
    snd3 (_,y,_) = y
    trd3 (_,_,z) = z

ricciTensor :: MetricTensor -> (Double, Double, Double) -> Matrix Double
ricciTensor metric point = 
    let riemann = riemannTensor metric point
    in (3><3) [sum [riemann ! (i,j,i,k) | i <- [0,1,2]]
              | j <- [0,1,2], k <- [0,1,2]]

scalarCurvature :: MetricTensor -> (Double, Double, Double) -> Double
scalarCurvature metric point = 
    let ricci = ricciTensor metric point
        ginv = metricInverse metric point
    in sum [ginv @@> (i,j) * ricci @@> (i,j) | i <- [0,1,2], j <- [0,1,2]]

einsteinTensor :: MetricTensor -> (Double, Double, Double) -> Matrix Double
einsteinTensor metric point = 
    let ricci = ricciTensor metric point
        r = scalarCurvature metric point
        g = metricComponents metric point
    in ricci - scale (r/2) g

stressEnergyTensor :: AnholonomicField -> (Double, Double, Double) -> Matrix Double
stressEnergyTensor field point = 
    let energy = energyDensity field point
        pressure = energy / 3  -- Simple equation of state
        g = metricComponents (inducedMetric field) point
    in scale energy g + scale pressure (ident 3 - g)

data Array a = Array ((Int,Int,Int), (Int,Int,Int)) [a]

listArray :: ((Int,Int,Int), (Int,Int,Int)) -> [a] -> Array a
listArray bounds elems = Array bounds elems

(!) :: Array a -> (Int,Int,Int) -> a
(!) (Array _ elems) (i,j,k) = elems !! (i*9 + j*3 + k)

instance Functor Array where
    fmap f (Array bounds elems) = Array bounds (map f elems)