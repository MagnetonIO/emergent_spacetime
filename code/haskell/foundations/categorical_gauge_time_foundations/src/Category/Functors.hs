{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Category.Functors where

import Category.Base
import Category.GaugeTime
import Category.Relational
import Category.Constraint
import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Set as Set

data BridgeFunctor = BridgeFunctor
    { bridgeObjectMap :: Configuration -> QuantumSubsystem
    , bridgeMorphismMap :: GaugeTransformation -> Correlation
    }

constructBridgeFunctor :: BridgeFunctor
constructBridgeFunctor = BridgeFunctor
    { bridgeObjectMap = configToQuantum
    , bridgeMorphismMap = gaugeToCorrelation
    }

configToQuantum :: Configuration -> QuantumSubsystem
configToQuantum config = QuantumSubsystem
    { subsystemDimension = length (configFields config)
    , subsystemBasis = map fieldToBasis (configFields config)
    , subsystemLabel = "Config" ++ show (configIndex config)
    }
  where
    fieldToBasis field = fromList [fieldComponent field :+ 0 | fieldComponent <- [const 1.0]]

gaugeToCorrelation :: GaugeTransformation -> Correlation
gaugeToCorrelation gauge = Correlation
    { corrDensityMatrix = gaugeToUnitary gauge
    , corrSubsystemA = QuantumSubsystem 2 [] "A"
    , corrSubsystemB = QuantumSubsystem 2 [] "B"
    }

gaugeToUnitary :: GaugeTransformation -> Matrix (Complex Double)
gaugeToUnitary gauge = gaugeMatrix gauge

data ObservableFunctor = ObservableFunctor
    { obsObjectMap :: Configuration -> Set.Set Double
    , obsMorphismMap :: GaugeTransformation -> (Set.Set Double -> Set.Set Double)
    }

constructObservableFunctor :: [Observable Double] -> ObservableFunctor
constructObservableFunctor observables = ObservableFunctor
    { obsObjectMap = \config -> Set.fromList [obsFunction obs config | obs <- observables]
    , obsMorphismMap = const id
    }

data WDWFunctor = WDWFunctor
    { wdwObjectMap :: ThreeGeometry -> PhysicalState
    , wdwMorphismMap :: ThreeDiffeomorphism -> ConstraintPreservingMap
    }

data ThreeGeometry = ThreeGeometry
    { threeMetric :: Matrix Double
    , threeExtrinsicCurvature :: Matrix Double
    }

data ThreeDiffeomorphism = ThreeDiff
    { diffeoMap :: ThreeGeometry -> ThreeGeometry
    }

quantizationFunctor :: ThreeGeometry -> PhysicalState
quantizationFunctor geom = PhysicalState
    { stateVector = fromList [1 :+ 0]
    , stateConstraints = [hamiltonianConstraint geom, momentumConstraint geom]
    , stateLabel = "WDW"
    }

hamiltonianConstraint :: ThreeGeometry -> Matrix (Complex Double)
hamiltonianConstraint _ = ident 1

momentumConstraint :: ThreeGeometry -> Matrix (Complex Double)
momentumConstraint _ = ident 1

data ClockSystemFunctor = ClockSystemFunctor
    { clockDecompose :: PhysicalState -> (QuantumSubsystem, QuantumSubsystem)
    , clockEntangle :: (QuantumSubsystem, QuantumSubsystem) -> PhysicalState
    }

decomposeIntoClockSystem :: PhysicalState -> ClockSystem
decomposeIntoClockSystem state = ClockSystem
    { clockStates = [stateVector state]
    , systemStates = mempty
    , entangledState = stateVector state
    }

data ReferenceFram eFunctor = ReferenceFrameFunctor
    { frameChoice :: PhysicalState -> QuantumSubsystem
    , frameTransform :: QuantumSubsystem -> QuantumSubsystem -> Matrix (Complex Double)
    }

selectReferenceFrame :: PhysicalState -> QuantumSubsystem
selectReferenceFrame _ = QuantumSubsystem 2 [] "Clock"

data HolographicFunctor = HolographicFunctor
    { bulkToTimeless :: BulkState -> PhysicalState
    , boundaryRGFlow :: BoundaryTheory -> TemporalPath
    }

data BulkState = BulkState
    { bulkGeometry :: ThreeGeometry
    , bulkMatter :: Vector (Complex Double)
    }

data BoundaryTheory = BoundaryTheory
    { boundaryOperators :: [Matrix (Complex Double)]
    , boundaryBeta :: Double
    }

holographicTime :: HolographicFunctor -> BulkState -> TemporalPath
holographicTime functor bulk = TemporalPath
    { pathParameter = const (Configuration [] (ident 4) 0)
    , pathInterval = (0, 1)
    , pathGaugeClass = []
    }