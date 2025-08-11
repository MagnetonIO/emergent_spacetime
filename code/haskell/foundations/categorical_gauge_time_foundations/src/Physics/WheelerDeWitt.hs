{-# LANGUAGE RecordWildCards #-}

module Physics.WheelerDeWitt where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map

type WaveFunction = Vector (Complex Double)
type SupermetricSpace = Matrix Double
type ThreeMetric = Matrix Double

data WheelerDeWittEquation = WheelerDeWittEquation
    { wdwHamiltonian :: Matrix (Complex Double)
    , wdwSupermetric :: SupermetricSpace
    , wdwMatterHamiltonian :: Matrix (Complex Double)
    , wdwCosmologicalConstant :: Double
    }

data UniverseWaveFunction = UniverseWaveFunction
    { uwfAmplitude :: ThreeMetric -> Complex Double
    , uwfNormalization :: Double
    , uwfBoundaryConditions :: BoundaryCondition
    }

data BoundaryCondition 
    = NoBounda ryCondition
    | HartleHawking
    | Tunneling
    | Vilenkin
    deriving (Eq, Show)

constructWDW :: SupermetricSpace -> Matrix (Complex Double) -> Double -> WheelerDeWittEquation
constructWDW supermetric matterHam lambda = WheelerDeWittEquation
    { wdwHamiltonian = geometricHamiltonian supermetric + matterHam
    , wdwSupermetric = supermetric
    , wdwMatterHamiltonian = matterHam
    , wdwCosmologicalConstant = lambda
    }

geometricHamiltonian :: SupermetricSpace -> Matrix (Complex Double)
geometricHamiltonian supermetric =
    let dim = rows supermetric
    in konst 0 (dim, dim)

applyWDWConstraint :: WheelerDeWittEquation -> WaveFunction -> WaveFunction
applyWDWConstraint WheelerDeWittEquation{..} psi =
    wdwHamiltonian #> psi

checkPhysicalState :: WheelerDeWittEquation -> WaveFunction -> Bool
checkPhysicalState wdw psi =
    let result = applyWDWConstraint wdw psi
    in norm_2 result < 1e-10

minisuperspace :: Int -> WheelerDeWittEquation
minisuperspace n = WheelerDeWittEquation
    { wdwHamiltonian = ident n
    , wdwSupermetric = ident n
    , wdwMatterHamiltonian = konst 0 (n, n)
    , wdwCosmologicalConstant = 0
    }

data DeWittMetric = DeWittMetric
    { dwMetricComponents :: Map.Map (Int, Int, Int, Int) Double
    , dwSignature :: (Int, Int)
    , dwDimension :: Int
    }

constructDeWittMetric :: Int -> DeWittMetric
constructDeWittMetric dim = DeWittMetric
    { dwMetricComponents = Map.empty
    , dwSignature = (dim * (dim + 1) `div` 2 - 1, 1)
    , dwDimension = dim
    }

canonicalMomentum :: ThreeMetric -> Matrix Double
canonicalMomentum metric =
    let dim = rows metric
        extrinsic = konst 0 (dim, dim)
    in sqrt (det metric) * (tr metric * ident dim - metric)

data QuantumCosmology = QuantumCosmology
    { qcScaleFactor :: Double -> Double
    , qcWaveFunction :: Double -> Complex Double
    , qcPotential :: Double -> Double
    , qcClassicalLimit :: Double -> Double
    }

friedmannWDW :: Double -> Double -> QuantumCosmology
friedmannWDW k lambda = QuantumCosmology
    { qcScaleFactor = id
    , qcWaveFunction = \a -> exp (i * sqrt (lambda / 3) * a^3 / 3)
    , qcPotential = \a -> k / a^2 - lambda * a^2
    , qcClassicalLimit = \a -> sqrt (lambda / 3) * a
    }
  where
    i = 0 :+ 1

hartleHawkingWaveFunction :: Double -> ThreeMetric -> Complex Double
hartleHawkingWaveFunction lambda metric =
    let action = euclideanAction lambda metric
    in exp (-action :+ 0)

euclideanAction :: Double -> ThreeMetric -> Double
euclideanAction lambda metric =
    let volume = sqrt (det metric)
        scalar = 0
    in volume * (scalar - 2 * lambda)

tunnelingWaveFunction :: Double -> ThreeMetric -> Complex Double
tunnelingWaveFunction lambda metric =
    let action = lorentzianAction lambda metric
    in exp (0 :+ action)

lorentzianAction :: Double -> ThreeMetric -> Double
lorentzianAction lambda metric =
    let volume = sqrt (abs $ det metric)
        scalar = 0
    in volume * (scalar - 2 * lambda)

semiclassicalApproximation :: WheelerDeWittEquation -> ThreeMetric -> (Complex Double, Complex Double)
semiclassicalApproximation wdw metric =
    let classical = classicalAction metric
        quantum = quantumCorrection wdw metric
    in (exp (i * classical), quantum)
  where
    i = 0 :+ 1
    classicalAction _ = 1.0
    quantumCorrection _ _ = 0.1 :+ 0

pathIntegralFormulation :: [ThreeMetric] -> WaveFunction
pathIntegralFormulation metrics =
    let dim = rows (head metrics)
        amplitudes = [exp (i * metricAction m) | m <- metrics]
    in fromList amplitudes / fromIntegral (length metrics)
  where
    i = 0 :+ 1
    metricAction m = tr m