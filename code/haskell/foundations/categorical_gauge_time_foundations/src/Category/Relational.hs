{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Category.Relational where

import Category.Base
import Numeric.LinearAlgebra hiding (tr)
import Data.Complex
import qualified Data.Map as Map

type HilbertSpace = Matrix (Complex Double)
type DensityMatrix = Matrix (Complex Double)

data QuantumSubsystem = QuantumSubsystem
    { subsystemDimension :: Int
    , subsystemBasis :: [Vector (Complex Double)]
    , subsystemLabel :: String
    } deriving (Show)

data Correlation = Correlation
    { corrDensityMatrix :: DensityMatrix
    , corrSubsystemA :: QuantumSubsystem
    , corrSubsystemB :: QuantumSubsystem
    } deriving (Show)

instance Category RelationalCategory where
    type Ob RelationalCategory = QuantumSubsystem
    type Mor RelationalCategory = Correlation
    
    idMor = undefined
    compose = composeCorrelations
    source corr = corrSubsystemA corr
    target corr = corrSubsystemB corr

data RelationalCategory

composeCorrelations :: Correlation -> Correlation -> Correlation
composeCorrelations corr2 corr1 = Correlation
    { corrDensityMatrix = partialTrace 2 compositeMatrix
    , corrSubsystemA = corrSubsystemA corr1
    , corrSubsystemB = corrSubsystemB corr2
    }
  where
    compositeMatrix = kronecker (corrDensityMatrix corr1) (corrDensityMatrix corr2)

partialTrace :: Int -> Matrix (Complex Double) -> Matrix (Complex Double)
partialTrace subsystem matrix = 
    let dim = rows matrix
        subdim = round $ sqrt $ fromIntegral dim
    in sumElements $ map (traceOutSubsystem subsystem subdim) [0..subdim-1]
  where
    traceOutSubsystem _ _ _ = matrix
    sumElements = foldr1 (+)

computeEntanglementEntropy :: DensityMatrix -> Double
computeEntanglementEntropy rho =
    let eigenvalues = toList $ eigenvaluesSH rho
        nonZero = filter (> 1e-10) $ map realPart eigenvalues
    in -sum [p * log p | p <- nonZero]

data ClockSystem = ClockSystem
    { clockStates :: [Vector (Complex Double)]
    , systemStates :: Map.Map Double (Vector (Complex Double))
    , entangledState :: Vector (Complex Double)
    }

pageWoottersMechanism :: ClockSystem -> Double -> Vector (Complex Double)
pageWoottersMechanism ClockSystem{..} time =
    case Map.lookup time systemStates of
        Just state -> state
        Nothing -> fromList [0]

conditionalState :: Vector (Complex Double) -> Int -> Vector (Complex Double) -> Vector (Complex Double)
conditionalState fullState clockIndex clockState =
    let dim = size fullState
        clockDim = size clockState
        systemDim = dim `div` clockDim
    in extractSystemState fullState clockIndex systemDim
  where
    extractSystemState _ _ _ = fullState

data CorrelationStructure = CorrelationStructure
    { corrMatrix :: Map.Map (String, String) DensityMatrix
    , corrGraph :: [(String, String)]
    , corrComplexity :: Double
    }

buildCorrelationStructure :: [QuantumSubsystem] -> CorrelationStructure
buildCorrelationStructure subsystems = CorrelationStructure
    { corrMatrix = Map.empty
    , corrGraph = [(subsystemLabel s1, subsystemLabel s2) | 
                   s1 <- subsystems, s2 <- subsystems, s1 /= s2]
    , corrComplexity = 0.0
    }

mutualInformation :: DensityMatrix -> DensityMatrix -> DensityMatrix -> Double
mutualInformation rhoA rhoB rhoAB =
    computeEntanglementEntropy rhoA + computeEntanglementEntropy rhoB - computeEntanglementEntropy rhoAB

relationalTime :: CorrelationStructure -> Double
relationalTime corr = corrComplexity corr