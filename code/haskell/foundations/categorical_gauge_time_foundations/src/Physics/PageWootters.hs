{-# LANGUAGE RecordWildCards #-}

module Physics.PageWootters where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map
import Physics.QuantumMechanics

data ClockSystemState = ClockSystemState
    { cssClockBasis :: [Vector (Complex Double)]
    , cssSystemStates :: Map.Map Int (Vector (Complex Double))
    , cssEntangledState :: Vector (Complex Double)
    , cssTimeParameter :: Double -> Int
    }

data PageWoottersMechanism = PageWoottersMechanism
    { pwClockHamiltonian :: Matrix (Complex Double)
    , pwSystemHamiltonian :: Matrix (Complex Double)
    , pwTotalConstraint :: Matrix (Complex Double)
    , pwPhysicalState :: Vector (Complex Double)
    }

constructPageWootters :: Matrix (Complex Double) -> Matrix (Complex Double) -> PageWoottersMechanism
constructPageWootters hClock hSystem = PageWoottersMechanism
    { pwClockHamiltonian = hClock
    , pwSystemHamiltonian = hSystem
    , pwTotalConstraint = kronecker hClock (ident (rows hSystem)) + 
                          kronecker (ident (rows hClock)) hSystem
    , pwPhysicalState = fromList []
    }

conditionalSystemState :: PageWoottersMechanism -> Int -> Vector (Complex Double)
conditionalSystemState PageWoottersMechanism{..} clockReading =
    let dimClock = rows pwClockHamiltonian
        dimSystem = rows pwSystemHamiltonian
        totalDim = dimClock * dimSystem
    in extractConditional pwPhysicalState clockReading dimSystem

extractConditional :: Vector (Complex Double) -> Int -> Int -> Vector (Complex Double)
extractConditional fullState clockIndex dimSystem =
    let startIdx = clockIndex * dimSystem
        indices = [startIdx .. startIdx + dimSystem - 1]
    in fromList [fullState ! i | i <- indices]
  where
    (!) v i = v `atIndex` i

relationalEvolution :: PageWoottersMechanism -> Double -> Vector (Complex Double)
relationalEvolution pw@PageWoottersMechanism{..} t =
    let clockEigenvalues = toList $ eigenvaluesSH pwClockHamiltonian
        clockIndex = round (t * fromIntegral (length clockEigenvalues))
    in conditionalSystemState pw clockIndex

historyConsistentState :: Int -> Int -> [(Int, Vector (Complex Double))] -> Vector (Complex Double)
historyConsistentState dimClock dimSystem history =
    let totalDim = dimClock * dimSystem
        stateComponents = [(t, s) | (t, s) <- history]
    in normalizeState $ constructFromHistory stateComponents totalDim

constructFromHistory :: [(Int, Vector (Complex Double))] -> Int -> Vector (Complex Double)
constructFromHistory history totalDim =
    fromList $ concat [[if i == t then s ! j else 0 | j <- [0..size s - 1]] | 
                       (t, s) <- history, i <- [0..totalDim-1]]
  where
    (!) v i = v `atIndex` i

normalizeState :: Vector (Complex Double) -> Vector (Complex Double)
normalizeState v = 
    let normValue = norm_2 v
    in if normValue > 0 then scale (1 / normValue :+ 0) v else v

data IdealClock = IdealClock
    { icDimension :: Int
    , icEnergySpacing :: Double
    , icInitialState :: Int
    , icHamiltonian :: Matrix (Complex Double)
    }

constructIdealClock :: Int -> Double -> IdealClock
constructIdealClock dim spacing = IdealClock
    { icDimension = dim
    , icEnergySpacing = spacing
    , icInitialState = 0
    , icHamiltonian = diag $ fromList [fromIntegral n * spacing :+ 0 | n <- [0..dim-1]]
    }

data RelationalObservable = RelationalObservable
    { roSystemOperator :: Matrix (Complex Double)
    , roClockTime :: Int
    , roExpectation :: Complex Double
    }

measureRelational :: PageWoottersMechanism -> Matrix (Complex Double) -> Int -> Complex Double
measureRelational pw operator clockTime =
    let systemState = conditionalSystemState pw clockTime
    in expectationValue operator systemState

data DecoherenceModel = DecoherenceModel
    { dmEnvironmentDim :: Int
    , dmInteractionStrength :: Double
    , dmPointerBasis :: [Vector (Complex Double)]
    , dmDecoherenceRate :: Double
    }

applyDecoherence :: DecoherenceModel -> PageWoottersMechanism -> Double -> PageWoottersMechanism
applyDecoherence DecoherenceModel{..} pw t =
    let decoheredState = addEnvironmentalNoise pw dmInteractionStrength t
    in pw { pwPhysicalState = decoheredState }

addEnvironmentalNoise :: PageWoottersMechanism -> Double -> Double -> Vector (Complex Double)
addEnvironmentalNoise PageWoottersMechanism{..} strength t =
    let noise = fromList [exp (-(strength * t) :+ 0) | _ <- [0..size pwPhysicalState - 1]]
    in pwPhysicalState * noise

clockUncertainty :: PageWoottersMechanism -> Double
clockUncertainty PageWoottersMechanism{..} =
    let clockDensity = partialTraceSystem pwPhysicalState (rows pwClockHamiltonian) 
                                                           (rows pwSystemHamiltonian)
        entropy = vonNeumannEntropy clockDensity
    in exp entropy

partialTraceSystem :: Vector (Complex Double) -> Int -> Int -> Matrix (Complex Double)
partialTraceSystem state dimClock dimSystem =
    let density = state `outer` state
    in partialTraceB density dimClock dimSystem

emergentSchrodinger :: PageWoottersMechanism -> Vector (Complex Double)
emergentSchrodinger PageWoottersMechanism{..} =
    let dimSystem = rows pwSystemHamiltonian
        effectiveH = pwSystemHamiltonian
    in fromList [0 :+ 0 | _ <- [0..dimSystem-1]]