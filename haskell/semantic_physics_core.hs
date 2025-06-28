{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticPhysics where

import Data.Complex
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Control.Monad.State
import Data.List (maximumBy)
import Data.Ord (comparing)

-- | Semantic value type
newtype Semantic = Semantic Double
  deriving (Show, Eq, Ord)

-- | Quantum state representation
data QuantumState = QuantumState
  { stateVector :: V.Vector (Complex Double)
  , dimension   :: Int
  } deriving (Show, Eq)

-- | Density matrix representation
data DensityMatrix = DensityMatrix
  { densityMatrix :: LA.Matrix (Complex Double)
  , matrixDim     :: Int
  } deriving (Show)

-- | Measurement operator
data MeasurementOp = MeasurementOp
  { measOp       :: LA.Matrix (Complex Double)
  , semanticVal  :: Semantic
  } deriving (Show)

-- | Information structure
data InfoStructure = InfoStructure
  { infoSet      :: V.Vector Semantic
  , semanticMetric :: Semantic -> Semantic -> Double
  }

-- | Matter configuration
data MatterConfig = MatterConfig
  { hilbertSpace :: Int
  , density      :: DensityMatrix
  , hamiltonian  :: LA.Matrix (Complex Double)
  }

-- | Create pure state from amplitudes
pureState :: [Complex Double] -> QuantumState
pureState amps = QuantumState
  { stateVector = V.fromList normalized
  , dimension = length amps
  }
  where
    norm = sqrt . sum $ map (\a -> realPart (a * conjugate a)) amps
    normalized = map (/ (norm :+ 0)) amps

-- | Convert pure state to density matrix
stateToDensity :: QuantumState -> DensityMatrix
stateToDensity (QuantumState vec dim) = DensityMatrix
  { densityMatrix = LA.outer v v'
  , matrixDim = dim
  }
  where
    v = LA.fromList $ V.toList vec
    v' = LA.conj v

-- | Semantic distance function
semanticDistance :: Semantic -> Semantic -> Double
semanticDistance (Semantic s1) (Semantic s2) = abs (s1 - s2)

-- | Information-Matter functor F
infoToMatter :: InfoStructure -> MatterConfig
infoToMatter (InfoStructure sems metric) = MatterConfig
  { hilbertSpace = V.length sems
  , density = createDensity sems
  , hamiltonian = createHamiltonian sems metric
  }
  where
    createDensity :: V.Vector Semantic -> DensityMatrix
    createDensity s = DensityMatrix
      { densityMatrix = LA.diag $ LA.fromList probs
      , matrixDim = V.length s
      }
      where
        beta = 1.0  -- Inverse temperature
        energies = V.map (\(Semantic x) -> x) s
        partition = V.sum $ V.map (\e -> exp (-beta * e)) energies
        probs = V.toList $ V.map (\e -> exp (-beta * e) / partition :+ 0) energies

    createHamiltonian :: V.Vector Semantic -> (Semantic -> Semantic -> Double) -> LA.Matrix (Complex Double)
    createHamiltonian s metric = LA.build (n, n) buildElem
      where
        n = V.length s
        buildElem i j = (-(metric (s V.! i) (s V.! j))) :+ 0

-- | Matter-Information functor G (adjoint)
matterToInfo :: MatterConfig -> InfoStructure
matterToInfo (MatterConfig dim density ham) = InfoStructure
  { infoSet = extractSemantics density
  , semanticMetric = inducedMetric ham
  }
  where
    extractSemantics :: DensityMatrix -> V.Vector Semantic
    extractSemantics (DensityMatrix dm _) = 
      V.fromList $ map (Semantic . realPart) $ LA.takeDiag dm
    
    inducedMetric :: LA.Matrix (Complex Double) -> Semantic -> Semantic -> Double
    inducedMetric h (Semantic s1) (Semantic s2) = 
      abs $ realPart (h LA.! (round s1, round s2))

-- | Semantic measurement
semanticMeasure :: [MeasurementOp] -> DensityMatrix -> (Int, DensityMatrix, Double)
semanticMeasure ops rho = (outcome, postState, probability)
  where
    -- Calculate probabilities
    probs = map (calcProb rho) ops
    
    -- Select outcome probabilistically (here we just take max for determinism)
    (outcome, maxProb) = maximumBy (comparing snd) $ zip [0..] probs
    probability = maxProb
    
    -- Post-measurement state
    postState = collapse (ops !! outcome) rho probability
    
    calcProb :: DensityMatrix -> MeasurementOp -> Double
    calcProb (DensityMatrix dm _) (MeasurementOp m _) =
      realPart $ LA.trace (m LA.<> LA.tr' m LA.<> dm)
    
    collapse :: MeasurementOp -> DensityMatrix -> Double -> DensityMatrix
    collapse (MeasurementOp m _) (DensityMatrix dm d) prob = DensityMatrix
      { densityMatrix = LA.scale (1 / (prob :+ 0)) collapsed
      , matrixDim = d
      }
      where
        collapsed = m LA.<> dm LA.<> LA.tr' m

-- | Create semantic superposition
semanticSuperposition :: [(Complex Double, Semantic)] -> QuantumState
semanticSuperposition weighted = pureState amplitudes
  where
    amplitudes = map fst weighted

-- | Semantic coherence measure
semanticCoherence :: QuantumState -> Double
semanticCoherence state = sum [calcCoherence i j | i <- [0..n-1], j <- [0..n-1], i /= j]
  where
    n = dimension state
    vec = stateVector state
    calcCoherence i j = 
      let ai = vec V.! i
          aj = vec V.! j
      in abs (realPart (ai * conjugate aj))

-- | Semantic projection operator
semanticProjector :: [Semantic] -> LA.Matrix (Complex Double)
semanticProjector sems = sum $ map projOnto [0..n-1]
  where
    n = length sems
    projOnto i = LA.outer basis basis
      where
        basis = LA.fromList [if j == i then 1 else 0 | j <- [0..n-1]]

-- | Evolution under semantic Hamiltonian
semanticEvolution :: Double -> MatterConfig -> MatterConfig
semanticEvolution t config = config { density = evolvedDensity }
  where
    h = hamiltonian config
    u = LA.expm $ LA.scale (0 :+ (-t)) h  -- U = exp(-iHt)
    DensityMatrix dm d = density config
    evolvedDensity = DensityMatrix
      { densityMatrix = u LA.<> dm LA.<> LA.tr' u
      , matrixDim = d
      }

-- | Find semantic fixed points
findFixedPoints :: MatterConfig -> [DensityMatrix]
findFixedPoints config = map makeFixed eigenspaces
  where
    h = hamiltonian config
    (eigvals, eigvecs) = LA.eig h
    eigenspaces = LA.toColumns eigvecs
    makeFixed evec = DensityMatrix
      { densityMatrix = LA.outer evec (LA.conj evec)
      , matrixDim = hilbertSpace config
      }

-- | Check if state is classical (diagonal in semantic basis)
isClassical :: DensityMatrix -> Double -> Bool
isClassical (DensityMatrix dm _) tolerance = 
  all (< tolerance) offDiagonals
  where
    n = LA.rows dm
    offDiagonals = [abs (realPart (dm LA.! (i,j))) | 
                    i <- [0..n-1], j <- [0..n-1], i /= j]

-- | Semantic basis transformation
toSemanticBasis :: MatterConfig -> DensityMatrix -> DensityMatrix
toSemanticBasis config (DensityMatrix dm d) = DensityMatrix
  { densityMatrix = v LA.<> dm LA.<> LA.tr' v
  , matrixDim = d
  }
  where
    h = hamiltonian config
    (_, v) = LA.eig h  -- Columns are eigenvectors