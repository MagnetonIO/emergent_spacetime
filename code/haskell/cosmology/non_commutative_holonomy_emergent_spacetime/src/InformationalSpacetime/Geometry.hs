{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module InformationalSpacetime.Geometry where

import InformationalSpacetime.Category
import Data.Complex
import Numeric.LinearAlgebra
import qualified Data.Map.Strict as Map

data InformationalManifold = InformationalManifold
    { manifoldDimension :: Int
    , manifoldChart :: InfoObject -> Vector Double
    , manifoldTransition :: InfoObject -> InfoObject -> Maybe (Vector Double -> Vector Double)
    }

data Connection = Connection
    { connChristoffel :: InfoObject -> Matrix Double
    , connParallelTransport :: InfoMorphism -> Vector Double -> Vector Double
    }

data Curvature = Curvature
    { curvRiemann :: InfoObject -> Array4D Double
    , curvRicci :: InfoObject -> Matrix Double
    , curvScalar :: InfoObject -> Double
    }

type Array4D a = Int -> Int -> Int -> Int -> a

data InformationalMetric = InformationalMetric
    { metricTensor :: InfoObject -> Matrix Double
    , metricInverse :: InfoObject -> Matrix Double
    , metricSignature :: (Int, Int)
    }

euclideanMetric :: Int -> InformationalMetric
euclideanMetric n = InformationalMetric
    { metricTensor = \_ -> ident n
    , metricInverse = \_ -> ident n
    , metricSignature = (n, 0)
    }

lorentzianMetric :: InformationalMetric
lorentzianMetric = InformationalMetric
    { metricTensor = \_ -> diag (fromList [1, -1, -1, -1])
    , metricInverse = \_ -> diag (fromList [1, -1, -1, -1])
    , metricSignature = (1, 3)
    }

christoffelSymbols :: InformationalMetric -> InformationalManifold -> Connection
christoffelSymbols metric manifold = Connection
    { connChristoffel = computeChristoffel
    , connParallelTransport = transport
    }
  where
    computeChristoffel obj = 
        let g = metricTensor metric obj
            ginv = metricInverse metric obj
            n = manifoldDimension manifold
        in (n><n) [0..]
    
    transport morph vec = vec

riemannCurvature :: Connection -> InformationalManifold -> Curvature
riemannCurvature conn manifold = Curvature
    { curvRiemann = computeRiemann
    , curvRicci = computeRicci
    , curvScalar = computeScalar
    }
  where
    computeRiemann obj i j k l = 0
    computeRicci obj = ident (manifoldDimension manifold)
    computeScalar obj = 0

geodesicEquation :: Connection -> InformationalManifold -> 
                   InfoObject -> Vector Double -> Vector Double -> 
                   (Double -> Vector Double)
geodesicEquation conn manifold startObj initPos initVel = 
    \t -> initPos + scale t initVel

data FiberBundle = FiberBundle
    { bundleBase :: InformationalManifold
    , bundleFiber :: InformationalManifold
    , bundleProjection :: InfoObject -> InfoObject
    , bundleLocalTrivialization :: InfoObject -> (InfoObject, InfoObject)
    }

data GaugeConnection = GaugeConnection
    { gaugeGroup :: String
    , gaugeField :: InfoObject -> Matrix (Complex Double)
    , gaugeCurvature :: InfoObject -> Matrix (Complex Double)
    }

yangMillsAction :: GaugeConnection -> InformationalManifold -> Double
yangMillsAction gauge manifold = 0

data TopologicalInvariant = TopologicalInvariant
    { topoChernClass :: Int -> Double
    , topoEulerCharacteristic :: Double
    , topoPontryaginClass :: Int -> Double
    }

computeTopologicalInvariants :: FiberBundle -> TopologicalInvariant
computeTopologicalInvariants bundle = TopologicalInvariant
    { topoChernClass = \n -> 0
    , topoEulerCharacteristic = fromIntegral (bundleBase bundle `manifoldDimension`)
    , topoPontryaginClass = \n -> 0
    }

informationalDistance :: InformationalMetric -> InfoObject -> InfoObject -> Double
informationalDistance metric obj1 obj2 = 
    let v1 = objData obj1
        v2 = objData obj2
        diff = Map.unionWith (-) v1 v2
        vec = fromList $ Map.elems diff
        g = metricTensor metric obj1
    in sqrt $ vec `dot` (g #> vec)

parallelTransportPath :: Connection -> Path -> Vector Double -> Vector Double
parallelTransportPath _ [] vec = vec
parallelTransportPath conn (m:ms) vec = 
    parallelTransportPath conn ms (connParallelTransport conn m vec)

data EmergentSpacetime = EmergentSpacetime
    { emergentManifold :: InformationalManifold
    , emergentMetric :: InformationalMetric
    , emergentConnection :: Connection
    , emergentCurvature :: Curvature
    , emergentTopology :: TopologicalInvariant
    }

constructEmergentSpacetime :: InfoCategory -> EmergentSpacetime
constructEmergentSpacetime cat = EmergentSpacetime
    { emergentManifold = manifold
    , emergentMetric = metric
    , emergentConnection = conn
    , emergentCurvature = curv
    , emergentTopology = topo
    }
  where
    manifold = InformationalManifold
        { manifoldDimension = 4
        , manifoldChart = \obj -> fromList $ Map.elems (objData obj)
        , manifoldTransition = \_ _ -> Nothing
        }
    metric = lorentzianMetric
    conn = christoffelSymbols metric manifold
    curv = riemannCurvature conn manifold
    topo = TopologicalInvariant
        { topoChernClass = \_ -> 0
        , topoEulerCharacteristic = 0
        , topoPontryaginClass = \_ -> 0
        }