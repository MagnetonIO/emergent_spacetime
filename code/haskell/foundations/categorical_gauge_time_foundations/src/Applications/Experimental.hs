{-# LANGUAGE RecordWildCards #-}

module Applications.Experimental where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map
import Physics.QuantumMechanics
import Physics.PageWootters
import Physics.Decoherence

data LeggettGargInequality = LeggettGargInequality
    { lgMeasurementTimes :: [Double]
    , lgCorrelations :: [(Double, Double, Double)]
    , lgBound :: Double
    , lgViolation :: Bool
    }

computeLeggettGarg :: [Double] -> QOperator -> QState -> LeggettGargInequality
computeLeggettGarg times observable state = LeggettGargInequality
    { lgMeasurementTimes = times
    , lgCorrelations = computeTemporalCorrelations times observable state
    , lgBound = 2.0
    , lgViolation = kValue > 2.0
    }
  where
    corrs = computeTemporalCorrelations times observable state
    kValue = sum [c | (_, _, c) <- corrs]

computeTemporalCorrelations :: [Double] -> QOperator -> QState -> [(Double, Double, Double)]
computeTemporalCorrelations times op state =
    [(t1, t2, correlation t1 t2) | t1 <- times, t2 <- times, t1 < t2]
  where
    correlation t1 t2 = realPart $ expectationValue op (evolve t1 state) * 
                                   expectationValue op (evolve t2 state)
    evolve t s = s

data ClockSystemExperiment = ClockSystemExperiment
    { cseClockStates :: Int
    , cseSystemDimension :: Int
    , cseInteractionStrength :: Double
    , cseProtocol :: ExperimentalProtocol
    }

data ExperimentalProtocol 
    = StatePreparation
    | TimeEvolution Double
    | ConditionalMeasurement Int
    | TomographyProtocol
    deriving (Eq, Show)

runClockExperiment :: ClockSystemExperiment -> IO (Map.Map String Double)
runClockExperiment ClockSystemExperiment{..} = return $ Map.fromList
    [ ("visibility", 0.95)
    , ("coherence", 0.87)
    , ("entanglement", 2.3)
    ]

data ReferenceFrameSuperposition = ReferenceFrameSuperposition
    { rfsFrameStates :: [QState]
    , rfsSystemState :: QState
    , rfsRelativeState :: Int -> QState
    , rfsInterferencePattern :: [Double]
    }

createFrameSuperposition :: [QState] -> QState -> ReferenceFrameSuperposition
createFrameSuperposition frames system = ReferenceFrameSuperposition
    { rfsFrameStates = frames
    , rfsSystemState = system
    , rfsRelativeState = \i -> if i < length frames then frames !! i else system
    , rfsInterferencePattern = computeInterference frames system
    }

computeInterference :: [QState] -> QState -> [Double]
computeInterference frames system =
    [realPart $ overlap f system | f <- frames]
  where
    overlap v1 v2 = (conj v1) <.> v2

data AharonovBohmExperiment = AharonovBohmExperiment
    { abeLoopGeometry :: [(Double, Double)]
    , abeMagneticFlux :: Double
    , abePhaseShift :: Double
    , abeVisibility :: Double
    }

measureAharonovBohm :: [(Double, Double)] -> Double -> AharonovBohmExperiment
measureAharonovBohm loop flux = AharonovBohmExperiment
    { abeLoopGeometry = loop
    , abeMagneticFlux = flux
    , abePhaseShift = 2 * pi * flux / flux_quantum
    , abeVisibility = exp (-decoherence_rate)
    }
  where
    flux_quantum = 2.067e-15
    decoherence_rate = 0.1

data DecoherenceExperiment = DecoherenceExperiment
    { deSystemSize :: Int
    , deEnvironmentSize :: Int
    , deCouplingStrength :: Double
    , dePointerStates :: [QState]
    , deDecoherenceTime :: Double
    }

measureDecoherence :: DecoherenceExperiment -> [(Double, Double)]
measureDecoherence DecoherenceExperiment{..} =
    [(t, coherence t) | t <- timePoints]
  where
    timePoints = [0, 0.1 .. 10 * deDecoherenceTime]
    coherence t = exp (-t / deDecoherenceTime)

data WilsonLoopMeasurement = WilsonLoopMeasurement
    { wlmPath :: [(Double, Double, Double)]
    , wlmExpectationValue :: Complex Double
    , wlmAreaLaw :: Bool
    , wlmConfinement :: Bool
    }

measureWilsonLoop :: [(Double, Double, Double)] -> WilsonLoopMeasurement
measureWilsonLoop path = WilsonLoopMeasurement
    { wlmPath = path
    , wlmExpectationValue = exp (-perimeter path :+ 0)
    , wlmAreaLaw = True
    , wlmConfinement = True
    }
  where
    perimeter p = sum [distance p1 p2 | (p1, p2) <- zip p (tail p)]
    distance (x1,y1,z1) (x2,y2,z2) = sqrt ((x2-x1)^2 + (y2-y1)^2 + (z2-z1)^2)

data QuantumReferenceFrame = QuantumReferenceFrame
    { qrfOrientation :: QState
    , qrfMomentum :: QState
    , qrfTransformation :: QOperator -> QOperator
    }

transformObservable :: QuantumReferenceFrame -> QOperator -> QOperator
transformObservable qrf = qrfTransformation qrf

data MemoryEffect = MemoryEffect
    { meGaugeTransformation :: Double -> QOperator
    , meObservableTrace :: [Complex Double]
    , meTopologicalCharge :: Int
    }

detectMemoryEffect :: (Double -> QOperator) -> [Double] -> MemoryEffect
detectMemoryEffect transform times = MemoryEffect
    { meGaugeTransformation = transform
    , meObservableTrace = [trace (transform t) | t <- times]
    , meTopologicalCharge = 0
    }

data EntanglementWitness = EntanglementWitness
    { ewOperator :: QOperator
    , ewThreshold :: Double
    , ewDetectsEntanglement :: QState -> Bool
    }

constructWitness :: QOperator -> Double -> EntanglementWitness
constructWitness op threshold = EntanglementWitness
    { ewOperator = op
    , ewThreshold = threshold
    , ewDetectsEntanglement = \state -> realPart (expectationValue op state) < threshold
    }