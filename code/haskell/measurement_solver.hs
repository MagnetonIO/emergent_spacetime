{-# LANGUAGE RecordWildCards #-}

module MeasurementSolver where

import SemanticPhysics
import Data.Complex
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Control.Monad.State
import System.Random

-- | Measurement context
data MeasurementContext = MeasurementContext
  { measurementBasis :: [MeasurementOp]
  , decoherenceRate  :: Double
  , semanticThreshold :: Double
  } deriving (Show)

-- | Measurement result
data MeasurementResult = MeasurementResult
  { measuredValue    :: Semantic
  , resultIndex      :: Int
  , finalState       :: DensityMatrix
  , probability      :: Double
  , semanticFidelity :: Double
  } deriving (Show)

-- | Solve measurement problem using semantic collapse
solveMeasurement :: MeasurementContext -> DensityMatrix -> MeasurementResult
solveMeasurement MeasurementContext{..} initialState =
  let
    -- Apply semantic constraints
    constrainedState = applySemanticConstraints semanticThreshold initialState
    
    -- Perform measurement
    (idx, postState, prob) = semanticMeasure measurementBasis constrainedState
    
    -- Extract semantic value
    MeasurementOp _ semVal = measurementBasis !! idx
    
    -- Calculate fidelity
    fidelity = calculateSemanticFidelity initialState postState
    
  in MeasurementResult
    { measuredValue = semVal
    , resultIndex = idx
    , finalState = postState
    , probability = prob
    , semanticFidelity = fidelity
    }

-- | Apply semantic constraints to ensure measurement consistency
applySemanticConstraints :: Double -> DensityMatrix -> DensityMatrix
applySemanticConstraints threshold dm@(DensityMatrix mat dim) =
  DensityMatrix
    { densityMatrix = normalizeMatrix $ filterMatrix mat
    , matrixDim = dim
    }
  where
    filterMatrix :: LA.Matrix (Complex Double) -> LA.Matrix (Complex Double)
    filterMatrix m = LA.build (LA.rows m, LA.cols m) filterElem
      where
        filterElem i j
          | i == j = m LA.! (i, j)  -- Keep diagonal
          | abs (realPart (m LA.! (i, j))) < threshold = 0
          | otherwise = m LA.! (i, j)
    
    normalizeMatrix :: LA.Matrix (Complex Double) -> LA.Matrix (Complex Double)
    normalizeMatrix m = LA.scale (1 / tr :+ 0) m
      where
        tr = LA.trace m

-- | Calculate semantic fidelity between states
calculateSemanticFidelity :: DensityMatrix -> DensityMatrix -> Double
calculateSemanticFidelity (DensityMatrix dm1 _) (DensityMatrix dm2 _) =
  realPart $ LA.trace (sqrtM dm1 LA.<> dm2 LA.<> sqrtM dm1)
  where
    sqrtM :: LA.Matrix (Complex Double) -> LA.Matrix (Complex Double)
    sqrtM m = v LA.<> LA.diag sqrtEigvals LA.<> LA.tr' v
      where
        (eigvals, v) = LA.eig m
        sqrtEigvals = LA.cmap sqrt eigvals

-- | Create measurement basis from semantic values
createSemanticBasis :: [Semantic] -> [MeasurementOp]
createSemanticBasis semantics = zipWith createOp [0..] semantics
  where
    n = length semantics
    createOp :: Int -> Semantic -> MeasurementOp
    createOp idx sem = MeasurementOp
      { measOp = projector idx n
      , semanticVal = sem
      }
    
    projector :: Int -> Int -> LA.Matrix (Complex Double)
    projector i dim = LA.build (dim, dim) buildElem
      where
        buildElem r c
          | r == i && c == i = 1
          | otherwise = 0

-- | Continuous measurement with decoherence
continuousMeasurement :: Double -> MeasurementContext -> DensityMatrix -> [(Double, MeasurementResult)]
continuousMeasurement totalTime context@MeasurementContext{..} initial =
  scanl measureStep (0, initialResult) timeSteps
  where
    dt = 0.01  -- Time step
    timeSteps = [dt, 2*dt .. totalTime]
    
    initialResult = solveMeasurement context initial
    
    measureStep :: (Double, MeasurementResult) -> Double -> (Double, MeasurementResult)
    measureStep (_, prevResult) t =
      let
        -- Apply decoherence
        decohered = applyDecoherence decoherenceRate dt (finalState prevResult)
        
        -- Measure
        newResult = solveMeasurement context decohered
      in (t, newResult)

-- | Apply decoherence to density matrix
applyDecoherence :: Double -> Double -> DensityMatrix -> DensityMatrix
applyDecoherence rate dt (DensityMatrix dm dim) =
  DensityMatrix
    { densityMatrix = LA.build (n, n) decohereElem
    , matrixDim = dim
    }
  where
    n = LA.rows dm
    gamma = rate * dt
    
    decohereElem i j
      | i == j = dm LA.! (i, j)
      | otherwise = dm LA.! (i, j) * (exp (-gamma) :+ 0)

-- | Quantum Zeno effect via frequent semantic measurements
quantumZeno :: Double -> Int -> MeasurementContext -> DensityMatrix -> DensityMatrix
quantumZeno totalTime numMeasurements context initial =
  finalState $ last results
  where
    dt = totalTime / fromIntegral numMeasurements
    
    results = scanl performMeasurement initialResult [1..numMeasurements]
    initialResult = solveMeasurement context initial
    
    performMeasurement :: MeasurementResult -> Int -> MeasurementResult
    performMeasurement prev _ = solveMeasurement context (finalState prev)

-- | Born rule probability calculation with semantic correction
semanticBornRule :: MeasurementOp -> DensityMatrix -> Double
semanticBornRule (MeasurementOp m _) (DensityMatrix dm _) =
  realPart $ LA.trace (m LA.<> dm LA.<> LA.tr' m) * semanticCorrection
  where
    -- Semantic correction factor based on operator structure
    semanticCorrection = 1.0 / (1.0 + operatorEntropy m)
    
    operatorEntropy :: LA.Matrix (Complex Double) -> Double
    operatorEntropy op = - sum [p * log p | p <- probs, p > 0]
      where
        (eigvals, _) = LA.eig (op LA.<> LA.tr' op)
        total = LA.sumElements eigvals
        probs = map (\e -> realPart e / realPart total) $ LA.toList eigvals

-- | Weak measurement with post-selection
weakMeasurement :: Double -> MeasurementOp -> DensityMatrix -> Semantic -> (Complex Double, DensityMatrix)
weakMeasurement strength op@(MeasurementOp m _) initial postSelect =
  (weakValue, finalState postState)
  where
    -- Weak measurement operator
    weakOp = LA.ident (LA.rows m) + LA.scale (strength :+ 0) m
    
    -- Apply weak measurement
    weakState = DensityMatrix
      { densityMatrix = weakOp LA.<> densityMatrix initial LA.<> LA.tr' weakOp
      , matrixDim = matrixDim initial
      }
    
    -- Post-selection
    postOp = createPostSelectionOp postSelect (matrixDim initial)
    (_, postState, _) = semanticMeasure [postOp] weakState
    
    -- Calculate weak value
    weakValue = LA.trace (m LA.<> densityMatrix postState) / 
                LA.trace (densityMatrix postState)

-- | Create post-selection operator
createPostSelectionOp :: Semantic -> Int -> MeasurementOp
createPostSelectionOp sem@(Semantic val) dim = MeasurementOp
  { measOp = LA.build (dim, dim) buildElem
  , semanticVal = sem
  }
  where
    idx = min (dim - 1) $ max 0 $ round val
    buildElem i j
      | i == idx && j == idx = 1
      | otherwise = 0

-- | Measurement-induced phase transition
measurementPhaseTransition :: Double -> MeasurementContext -> DensityMatrix -> (Double, Bool)
measurementPhaseTransition measurementRate context initial =
  (entanglementEntropy finalDM, isClassical finalDM 1e-6)
  where
    -- Number of measurements based on rate
    numMeasurements = round (measurementRate * 100)
    
    -- Perform measurements
    finalResult = quantumZeno 1.0 numMeasurements context initial
    finalDM = finalResult
    
    -- Calculate entanglement entropy
    entanglementEntropy :: DensityMatrix -> Double
    entanglementEntropy (DensityMatrix dm _) = 
      - sum [p * log p | p <- eigenvalues, p > 1e-10]
      where
        (eigvals, _) = LA.eig dm
        eigenvalues = map realPart $ LA.toList eigvals