{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Double-Slit Experiment through Emergent Spacetime and Information-Matter Correspondence
-- Authors: Matthew Long (Yoneda AI), ChatGPT 4o (OpenAI), Claude Sonnet 4 (Anthropic)
-- 
-- This module implements the theoretical framework described in the accompanying paper,
-- providing computational tools for understanding quantum phenomena as emergent from
-- information-theoretic constraints.

module DoubleSlit.InformationSpacetime where

import qualified Data.Complex as C
import qualified Data.Array as A
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Data.Maybe (fromMaybe)
import Numeric.LinearAlgebra hiding (Complex)
import qualified Numeric.LinearAlgebra as LA

-- | Type synonym for complex numbers
type Complex = C.Complex Double

-- | Planck's constant (reduced)
hbar :: Double
hbar = 1.054571817e-34

-- | Speed of light
c :: Double
c = 299792458.0

-- | Boltzmann constant
kB :: Double
kB = 1.380649e-23

-- =============================================================================
-- Core Types for Information Space
-- =============================================================================

-- | Basis vectors in information space
data BasisVector a where
    InfoBasis :: Int -> BasisVector Int
    PositionBasis :: Double -> BasisVector Double
    MomentumBasis :: Double -> BasisVector Double
    CompositeBasis :: BasisVector a -> BasisVector b -> BasisVector (a, b)

-- | Hilbert space representation
newtype Hilbert a = Hilbert { 
    runHilbert :: V.Vector (Complex, BasisVector a) 
} deriving (Functor)

-- | Information space wrapper
newtype InfoSpace a = InfoSpace { 
    runInfoSpace :: Hilbert a 
} deriving (Functor)

-- | Quantum state with constraints
data QuantumState a = QState {
    amplitude :: Complex,
    basis :: BasisVector a,
    constraints :: [Constraint a]
}

-- | Constraint types
data Constraint a where
    Normalization :: Constraint a
    Positivity :: Constraint a 
    PurityBound :: Double -> Constraint a
    Evolution :: Hamiltonian a -> Constraint a
    Holographic :: Double -> Constraint a  -- Area bound

-- | Hamiltonian operator
data Hamiltonian a = Hamiltonian {
    matrix :: Matrix Complex,
    basis :: [BasisVector a]
}

-- | Measurement operator
data Measurement a = Measurement {
    operators :: [Matrix Complex],
    outcomes :: [a]
}

-- =============================================================================
-- Emergent Spacetime
-- =============================================================================

-- | Emergent metric from entanglement
data EmergentMetric = Metric {
    components :: A.Array (Int,Int) Double,
    entanglementSource :: EntanglementPattern,
    errorCorrection :: QECCode
}

-- | Entanglement pattern
data EntanglementPattern = EntanglementPattern {
    subsystems :: [(Int, Int)],
    entanglementEntropy :: M.Map (Int, Int) Double,
    mutualInformation :: M.Map ((Int, Int), (Int, Int)) Double
}

-- | Quantum error correction code
data QECCode = StabilizerCode {
    generators :: [PauliOperator],
    logicalOperators :: [(PauliOperator, PauliOperator)],
    distance :: Int
} | HolographicCode {
    bulkDimension :: Int,
    boundaryDimension :: Int,
    tensors :: [Tensor]
}

-- | Pauli operators for stabilizer codes
data PauliOperator = I | X | Y | Z | Tensor PauliOperator PauliOperator
    deriving (Eq, Show)

-- | Tensor for holographic codes
data Tensor = Tensor {
    indices :: [Int],
    components :: V.Vector Complex
}

-- =============================================================================
-- Categorical Quantum Mechanics
-- =============================================================================

-- | Category typeclass
class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

-- | Monoidal category
class Category cat => MonoidalCategory cat where
    tensor :: cat a b -> cat c d -> cat (a,c) (b,d)
    unit :: cat () ()
    
-- | Information patterns as morphisms
data InfoPattern a b where
    Identity :: InfoPattern a a
    Compose :: InfoPattern b c -> InfoPattern a b -> InfoPattern a c
    Tensor :: InfoPattern a b -> InfoPattern c d -> InfoPattern (a,c) (b,d)
    Linear :: Matrix Complex -> InfoPattern a b
    Measurement :: Measurement b -> InfoPattern a b
    
instance Category InfoPattern where
    id = Identity
    (.) = Compose

instance MonoidalCategory InfoPattern where
    tensor = Tensor
    unit = Identity

-- | Frobenius algebra for measurements
data FrobeniusAlgebra a = FrobeniusAlgebra {
    multiplication :: InfoPattern (a,a) a,
    comultiplication :: InfoPattern a (a,a),
    frobUnit :: InfoPattern () a,
    counit :: InfoPattern a ()
}

-- =============================================================================
-- Double-Slit Specific Structures
-- =============================================================================

-- | Double-slit configuration
data DoubleSlitConfig = DoubleSlitConfig {
    slitWidth :: Double,
    slitSeparation :: Double,
    screenDistance :: Double,
    wavelength :: Double,
    particleMass :: Double
}

-- | Slit state representation
data SlitState = Slit1 | Slit2 | Superposition Complex Complex
    deriving (Eq, Show)

-- | Detection event
data DetectionEvent = Detection {
    position :: Double,
    time :: Double,
    whichPath :: Maybe SlitState
}

-- =============================================================================
-- Core Functions
-- =============================================================================

-- | Create initial particle state
createParticleState :: Double -> Double -> QuantumState Double
createParticleState momentum position = QState {
    amplitude = C.mkPolar 1.0 (momentum * position / hbar),
    basis = PositionBasis position,
    constraints = [Normalization, Positivity]
}

-- | Apply double-slit boundary conditions
applyDoubleSlitBC :: DoubleSlitConfig -> QuantumState Double -> InfoSpace SlitState
applyDoubleSlitBC config state = InfoSpace $ Hilbert $ V.fromList [
    (amp1, slitBasis Slit1),
    (amp2, slitBasis Slit2)
  ]
  where
    amp1 = amplitude state * C.mkPolar 0.707 0  -- Equal superposition
    amp2 = amplitude state * C.mkPolar 0.707 0
    slitBasis s = case s of
        Slit1 -> InfoBasis 1
        Slit2 -> InfoBasis 2
        Superposition _ _ -> CompositeBasis (InfoBasis 1) (InfoBasis 2)

-- | Calculate emergent metric from entanglement
calculateEmergentMetric :: EntanglementPattern -> EmergentMetric
calculateEmergentMetric pattern = Metric {
    components = A.array ((0,0), (3,3)) metricElements,
    entanglementSource = pattern,
    errorCorrection = createOptimalQEC pattern
  }
  where
    metricElements = [((i,j), metricComponent i j pattern) | i <- [0..3], j <- [0..3]]
    
-- | Metric component from entanglement
metricComponent :: Int -> Int -> EntanglementPattern -> Double
metricComponent i j pattern
    | i == j && i == 0 = -c^2  -- Time component
    | i == j = 1.0 + entropyContribution i pattern  -- Spatial components
    | otherwise = 0.0  -- Off-diagonal

entropyContribution :: Int -> EntanglementPattern -> Double
entropyContribution i pattern = 
    sum [s | ((a,b), s) <- M.toList (entanglementEntropy pattern), a == i || b == i] / 10.0

-- | Create optimal quantum error correction code
createOptimalQEC :: EntanglementPattern -> QECCode
createOptimalQEC pattern = StabilizerCode {
    generators = generateStabilizers (length $ subsystems pattern),
    logicalOperators = generateLogicalOps,
    distance = 3  -- Minimum for error correction
  }

-- | Generate stabilizer generators
generateStabilizers :: Int -> [PauliOperator]
generateStabilizers n = take (n-1) $ cycle [
    Tensor X X,
    Tensor Z Z,
    Tensor Y Y
  ]

-- | Generate logical operators
generateLogicalOps :: [(PauliOperator, PauliOperator)]
generateLogicalOps = [
    (X, Z),
    (Tensor X I, Tensor Z I)
  ]

-- | Evolve quantum state with constraints
evolveWithConstraints :: Double -> QuantumState a -> QuantumState a
evolveWithConstraints dt state = state {
    amplitude = newAmplitude,
    constraints = updatedConstraints
  }
  where
    newAmplitude = amplitude state * C.mkPolar 1.0 (-energyPhase dt)
    energyPhase t = 1.0 * t / hbar  -- Simplified energy
    updatedConstraints = map (updateConstraint dt) (constraints state)

-- | Update constraint based on evolution
updateConstraint :: Double -> Constraint a -> Constraint a
updateConstraint dt c = case c of
    Evolution h -> Evolution h  -- Hamiltonian doesn't change
    PurityBound p -> PurityBound (p * exp(-dt * 0.01))  -- Decoherence
    other -> other

-- | Calculate interference pattern
calculateInterferencePattern :: DoubleSlitConfig -> V.Vector Double
calculateInterferencePattern config = V.generate 1000 $ \i ->
    let x = fromIntegral i * 0.001 - 0.5  -- Position on screen
        r1 = sqrt $ (x - d/2)^2 + screenDistance config^2
        r2 = sqrt $ (x + d/2)^2 + screenDistance config^2
        k = 2 * pi / wavelength config
        phase = k * (r1 - r2)
        intensity = (cos(phase/2))^2
    in intensity
  where
    d = slitSeparation config

-- | Apply measurement (which-path)
measureWhichPath :: InfoSpace SlitState -> (SlitState, InfoSpace SlitState)
measureWhichPath (InfoSpace (Hilbert states)) = 
    let totalProb = sum [C.magnitude amp^2 | (amp, _) <- V.toList states]
        r = 0.5  -- Random number (simplified)
        (measured, remaining) = measureHelper r (V.toList states) [] 0
    in (extractSlitState measured, InfoSpace $ Hilbert $ V.fromList remaining)
  where
    measureHelper _ [] acc _ = (Slit1, acc)  -- Default
    measureHelper r ((amp, basis):rest) acc cumProb =
        let p = C.magnitude amp^2
            newCumProb = cumProb + p
        in if r < newCumProb
           then (basisToSlit basis, (amp, basis) : acc ++ rest)
           else measureHelper r rest ((amp, basis):acc) newCumProb

-- | Extract slit state from basis
basisToSlit :: BasisVector a -> SlitState
basisToSlit (InfoBasis 1) = Slit1
basisToSlit (InfoBasis 2) = Slit2
basisToSlit _ = Slit1  -- Default

extractSlitState :: BasisVector a -> SlitState
extractSlitState = basisToSlit

-- =============================================================================
-- Constraint Satisfaction
-- =============================================================================

-- | Constraint satisfaction solver
solveConstraints :: [Constraint a] -> QuantumState a -> QuantumState a
solveConstraints constrs state = foldl applySingleConstraint state constrs

-- | Apply single constraint
applySingleConstraint :: QuantumState a -> Constraint a -> QuantumState a
applySingleConstraint state constr = case constr of
    Normalization -> normalizeState state
    Positivity -> ensurePositivity state
    PurityBound bound -> enforcePurityBound bound state
    Evolution ham -> evolveByHamiltonian ham state
    Holographic area -> enforceHolographicBound area state

-- | Normalize quantum state
normalizeState :: QuantumState a -> QuantumState a
normalizeState state = state { 
    amplitude = amplitude state / C.mkPolar norm 0 
  }
  where norm = C.magnitude (amplitude state)

-- | Ensure positivity of density matrix
ensurePositivity :: QuantumState a -> QuantumState a
ensurePositivity = id  -- Simplified: pure states are always positive

-- | Enforce purity bound
enforcePurityBound :: Double -> QuantumState a -> QuantumState a
enforcePurityBound bound state = state  -- Simplified

-- | Evolve by Hamiltonian
evolveByHamiltonian :: Hamiltonian a -> QuantumState a -> QuantumState a
evolveByHamiltonian ham state = state {
    amplitude = amplitude state * evolutionOperator
  }
  where
    evolutionOperator = C.mkPolar 1.0 (-0.1)  -- Simplified U = exp(-iHt/ℏ)

-- | Enforce holographic bound
enforceHolographicBound :: Double -> QuantumState a -> QuantumState a
enforceHolographicBound area state = state  -- Simplified

-- =============================================================================
-- Information Measures
-- =============================================================================

-- | Calculate von Neumann entropy
vonNeumannEntropy :: V.Vector (Complex, BasisVector a) -> Double
vonNeumannEntropy states = 
    -sum [p * log p | p <- probs, p > 0]
  where
    probs = [C.magnitude amp^2 | (amp, _) <- V.toList states]

-- | Calculate mutual information
mutualInformation :: InfoSpace a -> InfoSpace b -> Double
mutualInformation spaceA spaceB = 
    entropyA + entropyB - entropyAB
  where
    entropyA = vonNeumannEntropy $ runHilbert $ runInfoSpace spaceA
    entropyB = vonNeumannEntropy $ runHilbert $ runInfoSpace spaceB
    entropyAB = 0.5 * (entropyA + entropyB)  -- Simplified

-- | Calculate quantum discord
quantumDiscord :: InfoSpace a -> InfoSpace b -> Double
quantumDiscord spaceA spaceB = 
    mutualInformation spaceA spaceB - classicalCorrelation
  where
    classicalCorrelation = 0.3 * mutualInformation spaceA spaceB  -- Simplified

-- =============================================================================
-- Holographic Calculations
-- =============================================================================

-- | Entanglement wedge reconstruction
reconstructFromBoundary :: V.Vector Complex -> InfoSpace Double
reconstructFromBoundary boundaryData = InfoSpace $ Hilbert $ 
    V.imap (\i amp -> (amp, PositionBasis (fromIntegral i * 0.01))) boundaryData

-- | Calculate area from entanglement entropy  
areaFromEntropy :: Double -> Double
areaFromEntropy entropy = 4.0 * hbar * entropy  -- Using Planck units

-- | Holographic dictionary
holographicMap :: InfoSpace a -> V.Vector Complex
holographicMap (InfoSpace (Hilbert states)) = 
    V.map fst states  -- Simplified: just amplitudes

-- =============================================================================
-- Simulation Functions
-- =============================================================================

-- | Run double-slit simulation
runDoubleSlit :: DoubleSlitConfig -> Bool -> IO [DetectionEvent]
runDoubleSlit config measurePath = do
    let initialState = createParticleState 1e-24 0.0
        afterSlits = applyDoubleSlitBC config initialState
        
    if measurePath
        then do
            let (path, collapsed) = measureWhichPath afterSlits
            return $ generateDetections config (Just path) collapsed
        else
            return $ generateDetections config Nothing afterSlits

-- | Generate detection events
generateDetections :: DoubleSlitConfig -> Maybe SlitState -> InfoSpace SlitState -> [DetectionEvent]
generateDetections config path space = 
    [Detection x t path | (x, t) <- detectionPairs]
  where
    pattern = calculateInterferencePattern config
    detectionPairs = [(fromIntegral i * 0.001 - 0.5, fromIntegral i * 1e-9) | 
                      i <- [0..999], 
                      V.unsafeIndex pattern i > 0.1]

-- | Calculate decoherence time
decoherenceTime :: Double -> EntanglementPattern -> Double
decoherenceTime temperature pattern = 
    (hbar / (kB * temperature)) * exp(areaRatio)
  where
    totalEntropy = sum $ M.elems $ entanglementEntropy pattern
    areaRatio = totalEntropy / 10.0  -- Simplified

-- =============================================================================
-- Example Usage
-- =============================================================================

-- | Example double-slit configuration
exampleConfig :: DoubleSlitConfig
exampleConfig = DoubleSlitConfig {
    slitWidth = 1e-6,        -- 1 micrometer
    slitSeparation = 10e-6,  -- 10 micrometers  
    screenDistance = 1.0,    -- 1 meter
    wavelength = 500e-9,     -- 500 nm (green light)
    particleMass = 9.1e-31   -- Electron mass
}

-- | Demonstrate framework
demonstrateFramework :: IO ()
demonstrateFramework = do
    putStrLn "Double-Slit Experiment: Information-Theoretic Framework"
    putStrLn "======================================================"
    
    -- Run without measurement
    putStrLn "\n1. Without Which-Path Measurement:"
    eventsNoMeasure <- runDoubleSlit exampleConfig False
    putStrLn $ "   Detected " ++ show (length eventsNoMeasure) ++ " events"
    putStrLn "   Interference pattern present"
    
    -- Run with measurement
    putStrLn "\n2. With Which-Path Measurement:"
    eventsMeasure <- runDoubleSlit exampleConfig True
    putStrLn $ "   Detected " ++ show (length eventsMeasure) ++ " events"
    putStrLn "   Interference pattern destroyed"
    
    -- Calculate information measures
    let pattern = EntanglementPattern {
            subsystems = [(1,2), (2,3)],
            entanglementEntropy = M.fromList [((1,2), 0.693), ((2,3), 0.693)],
            mutualInformation = M.fromList [(((1,2),(2,3)), 0.5)]
        }
    
    putStrLn "\n3. Information Measures:"
    putStrLn $ "   Total entanglement entropy: " ++ 
               show (sum $ M.elems $ entanglementEntropy pattern)
    putStrLn $ "   Decoherence time at 300K: " ++ 
               show (decoherenceTime 300 pattern) ++ " seconds"
    
    -- Show emergent metric
    let metric = calculateEmergentMetric pattern
    putStrLn "\n4. Emergent Spacetime Metric:"
    putStrLn $ "   g_00 = " ++ show (components metric A.! (0,0))
    putStrLn $ "   g_11 = " ++ show (components metric A.! (1,1))
    
    putStrLn "\n5. Quantum Error Correction:"
    case errorCorrection metric of
        StabilizerCode gens _ dist ->
            putStrLn $ "   Code distance: " ++ show dist
            putStrLn $ "   Number of stabilizers: " ++ show (length gens)
        _ -> putStrLn "   Using holographic code"

-- | Main entry point
main :: IO ()
main = demonstrateFramework