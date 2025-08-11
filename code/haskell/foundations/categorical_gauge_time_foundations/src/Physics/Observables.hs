{-# LANGUAGE RecordWildCards #-}

module Physics.Observables where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map
import Physics.GaugeTheory
import Physics.QuantumMechanics
import Category.GaugeTime

data DiracObservable = DiracObservable
    { doOperator :: Matrix (Complex Double)
    , doCommutesWithConstraints :: [Matrix (Complex Double)] -> Bool
    , doClassicalLimit :: Maybe (Configuration -> Double)
    }

constructDiracObservable :: Matrix (Complex Double) -> DiracObservable
constructDiracObservable op = DiracObservable
    { doOperator = op
    , doCommutesWithConstraints = \constraints -> 
        all (\c -> norm_Frob (commutator op c) < 1e-10) constraints
    , doClassicalLimit = Nothing
    }

data GaugeInvariantQuantity a = GaugeInvariantQuantity
    { giqCompute :: Configuration -> a
    , giqVerifyInvariance :: GaugeTransformation -> Configuration -> Bool
    , giqPhysicalMeaning :: String
    }

wilsonLoopObservable :: [V3 Double] -> GaugeInvariantQuantity (Complex Double)
wilsonLoopObservable path = GaugeInvariantQuantity
    { giqCompute = \_ -> 1 :+ 0
    , giqVerifyInvariance = \_ _ -> True
    , giqPhysicalMeaning = "Wilson loop around path"
    }

data RelationalQuantity = RelationalQuantity
    { rqClockObservable :: Matrix (Complex Double)
    , rqSystemObservable :: Matrix (Complex Double)
    , rqCorrelation :: Matrix (Complex Double) -> Matrix (Complex Double) -> Double
    }

constructRelationalQuantity :: Matrix (Complex Double) -> Matrix (Complex Double) -> RelationalQuantity
constructRelationalQuantity clockOp systemOp = RelationalQuantity
    { rqClockObservable = clockOp
    , rqSystemObservable = systemOp
    , rqCorrelation = \c s -> realPart $ trace (c <> s) / fromIntegral (rows c)
    }

data EvolvedObservable = EvolvedObservable
    { eoInitialOperator :: Matrix (Complex Double)
    , eoHeisenbergPicture :: Double -> Matrix (Complex Double)
    , eoSchrodingerExpectation :: Double -> Vector (Complex Double) -> Complex Double
    }

heisenbergEvolution :: Matrix (Complex Double) -> Matrix (Complex Double) -> Double -> Matrix (Complex Double)
heisenbergEvolution hamiltonian operator t =
    let u = expm (scale (i * t) hamiltonian)
        uDagger = tr u
    in uDagger <> operator <> u
  where
    i = 0 :+ 1

data CompleteObservables = CompleteObservables
    { coOperators :: [Matrix (Complex Double)]
    , coCommutingSet :: Bool
    , coEigenspaces :: Map.Map [Complex Double] (Vector (Complex Double))
    }

findCompleteSet :: [Matrix (Complex Double)] -> Maybe CompleteObservables
findCompleteSet ops =
    if all commute [(o1, o2) | o1 <- ops, o2 <- ops, o1 /= o2]
    then Just $ CompleteObservables ops True Map.empty
    else Nothing
  where
    commute (a, b) = norm_Frob (commutator a b) < 1e-10

data PointerStates = PointerStates
    { psStates :: [Vector (Complex Double)]
    , psStability :: Double
    , psDecoherenceBasis :: Bool
    }

selectPointerStates :: Matrix (Complex Double) -> Double -> PointerStates
selectPointerStates interaction strength = PointerStates
    { psStates = eigenvectorsSH interaction
    , psStability = strength
    , psDecoherenceBasis = True
    }
  where
    eigenvectorsSH m = [flatten $ eigenvectors m ?? (All, Pos i) | i <- [0..rows m - 1]]
    eigenvectors = fromColumns . toColumns . snd . eigSH

data MeasurementRecord = MeasurementRecord
    { mrObservable :: Matrix (Complex Double)
    , mrOutcome :: Complex Double
    , mrPostMeasurementState :: Vector (Complex Double)
    , mrProbability :: Double
    }

performMeasurement :: Matrix (Complex Double) -> Vector (Complex Double) -> MeasurementRecord
performMeasurement observable state =
    let eigenvalues = toList $ eigenvaluesSH observable
        eigenvectors = eigenVectorsSH observable
        probabilities = [norm_2 (proj #> state)^2 | proj <- eigenvectors]
        maxIndex = maxIndex probabilities
        outcome = eigenvalues !! maxIndex
        postState = normalize $ eigenvectors !! maxIndex
    in MeasurementRecord observable outcome postState (probabilities !! maxIndex)
  where
    eigenVectorsSH m = [flatten $ eigenvectors m ?? (All, Pos i) | i <- [0..rows m - 1]]
    eigenvectors = fromColumns . toColumns . snd . eigSH
    maxIndex xs = snd $ maximum $ zip xs [0..]
    normalize v = scale (1 / norm_2 v :+ 0) v

data ConservedQuantity = ConservedQuantity
    { cqOperator :: Matrix (Complex Double)
    , cqConservationLaw :: Matrix (Complex Double) -> Bool
    , cqNoetherCharge :: Maybe Double
    }

checkConservation :: ConservedQuantity -> Matrix (Complex Double) -> Bool
checkConservation ConservedQuantity{..} hamiltonian =
    norm_Frob (commutator cqOperator hamiltonian) < 1e-10