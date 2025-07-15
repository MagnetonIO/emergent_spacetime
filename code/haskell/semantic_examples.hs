{-# LANGUAGE RecordWildCards #-}

module Examples where

import SemanticPhysics
import MeasurementSolver
import SuperpositionSolver
import Data.Complex
import qualified Numeric.LinearAlgebra as LA
import Control.Monad
import Text.Printf

-- | Example 1: Basic information-matter correspondence
example1_InfoMatterCorrespondence :: IO ()
example1_InfoMatterCorrespondence = do
  putStrLn "=== Example 1: Information-Matter Correspondence ==="
  
  -- Create information structure
  let semantics = [Semantic 0.0, Semantic 1.0, Semantic 2.0, Semantic 3.0]
      metric s1 s2 = exp (- semanticDistance s1 s2)
      infoStruct = InfoStructure (V.fromList semantics) metric
  
  -- Apply functor F
  let matterConfig = infoToMatter infoStruct
  putStrLn $ "Hilbert space dimension: " ++ show (hilbertSpace matterConfig)
  putStrLn $ "Density matrix trace: " ++ show (LA.trace $ densityMatrix $ density matterConfig)
  
  -- Apply adjoint G
  let recoveredInfo = matterToInfo matterConfig
  putStrLn $ "Recovered semantic values: " ++ show (V.length $ infoSet recoveredInfo)
  
  putStrLn ""

-- | Example 2: Solving measurement problem
example2_MeasurementProblem :: IO ()
example2_MeasurementProblem = do
  putStrLn "=== Example 2: Measurement Problem Solution ==="
  
  -- Create superposition state
  let state = pureState [1/sqrt 2 :+ 0, 1/sqrt 2 :+ 0]
      density = stateToDensity state
  
  -- Create measurement context
  let semantics = [Semantic 0.0, Semantic 1.0]
      basis = createSemanticBasis semantics
      context = MeasurementContext
        { measurementBasis = basis
        , decoherenceRate = 0.1
        , semanticThreshold = 1e-6
        }
  
  -- Solve measurement
  let result = solveMeasurement context density
  putStrLn $ "Measured semantic value: " ++ show (measuredValue result)
  putStrLn $ "Probability: " ++ printf "%.4f" (probability result)
  putStrLn $ "Semantic fidelity: " ++ printf "%.4f" (semanticFidelity result)
  
  -- Demonstrate quantum Zeno effect
  let zenoResult = quantumZeno 1.0 100 context density
  putStrLn $ "After Zeno (100 measurements): " ++ 
             if isClassical zenoResult 1e-6 then "Classical" else "Quantum"
  
  putStrLn ""

-- | Example 3: Superposition stability
example3_SuperpositionStability :: IO ()
example3_SuperpositionStability = do
  putStrLn "=== Example 3: Superposition Stability ==="
  
  -- Configuration
  let config = SuperpositionConfig
        { semanticBasis = [Semantic 0.0, Semantic 1.0, Semantic 2.0]
        , coherenceDecay = 0.05
        , entanglementRate = 0.1
        , temperatureParam = 0.5
        }
  
  -- Create semantic superposition
  let components = 
        [ (1/sqrt 3 :+ 0, Semantic 0.0)
        , (1/sqrt 3 :+ 0, Semantic 1.0)
        , (1/sqrt 3 :+ 0, Semantic 2.0)
        ]
      superpos = solveSuperposition config components
  
  putStrLn $ "Initial purity: " ++ printf "%.4f" (purity superpos)
  putStrLn $ "Semantic entanglement: " ++ printf "%.4f" (semanticEntanglement superpos)
  
  -- Evolve and check stability
  let evolved = evolveSuperposition 1.0 config superpos
  putStrLn $ "Purity after evolution: " ++ printf "%.4f" (purity evolved)
  
  -- Stabilize
  let stabilized = stabilizeSuperposition config evolved
  putStrLn $ "Purity after stabilization: " ++ printf "%.4f" (purity stabilized)
  
  putStrLn ""

-- | Example 4: Semantic Bell states and entanglement
example4_SemanticEntanglement :: IO ()
example4_SemanticEntanglement = do
  putStrLn "=== Example 4: Semantic Entanglement ==="
  
  -- Create semantic Bell state
  let bell = prepareSemanticBell (Semantic 0.0) (Semantic 1.0)
  putStrLn $ "Bell state purity: " ++ printf "%.4f" (purity bell)
  putStrLn $ "Bell state entanglement: " ++ printf "%.4f" (semanticEntanglement bell)
  
  -- Create GHZ state
  let ghz = prepareSemanticGHZ [Semantic k | k <- [0.0, 1.0, 2.0, 3.0]]
  putStrLn $ "GHZ state purity: " ++ printf "%.4f" (purity ghz)
  putStrLn $ "GHZ state entanglement: " ++ printf "%.4f" (semanticEntanglement ghz)
  
  -- Calculate interference
  let interference = calculateInterference bell ghz
  putStrLn $ "Interference pattern (first 3): " ++ 
             show (take 3 $ V.toList interference)
  
  putStrLn ""

-- | Example 5: Decoherence-free subspaces
example5_DecoherenceFree :: IO ()
example5_DecoherenceFree = do
  putStrLn "=== Example 5: Decoherence-Free Subspaces ==="
  
  let config = SuperpositionConfig
        { semanticBasis = [Semantic k | k <- [0.0, 0.1 .. 1.0]]
        , coherenceDecay = 0.01
        , entanglementRate = 0.5
        , temperatureParam = 0.1
        }
  
  -- Find decoherence-free subspaces
  let semantics = [Semantic 0.0, Semantic 0.5, Semantic 1.0, Semantic 1.5]
  case findDecohereenceFreeSubspace config semantics of
    Nothing -> putStrLn "No decoherence-free subspace found"
    Just subspaces -> do
      putStrLn $ "Found " ++ show (length subspaces) ++ " decoherence-free subspaces"
      forM_ (zip [1..] subspaces) $ \(i, subspace) -> do
        putStrLn $ "  Subspace " ++ show i ++ " purity: " ++ 
                   printf "%.4f" (purity subspace)
  
  putStrLn ""

-- | Example 6: Measurement-induced phase transition
example6_PhaseTransition :: IO ()
example6_PhaseTransition = do
  putStrLn "=== Example 6: Measurement-Induced Phase Transition ==="
  
  -- Initial entangled state
  let entangled = prepareSemanticGHZ [Semantic k | k <- [0.0, 1.0, 2.0]]
      density = DensityMatrix
        { densityMatrix = LA.outer v v'
        , matrixDim = V.length (amplitudes entangled)
        }
        where
          v = LA.fromList $ V.toList (amplitudes entangled)
          v' = LA.conj v
  
  -- Measurement context
  let semantics = [Semantic k | k <- [0.0, 1.0, 2.0]]
      basis = createSemanticBasis semantics
      context = MeasurementContext
        { measurementBasis = basis
        , decoherenceRate = 0.01
        , semanticThreshold = 1e-6
        }
  
  -- Test different measurement rates
  putStrLn "Measurement rate -> (Entropy, Classical?)"
  forM_ [0.1, 0.5, 1.0, 2.0, 5.0] $ \rate -> do
    let (entropy, classical) = measurementPhaseTransition rate context density
    putStrLn $ printf "  %.1f -> (%.4f, %s)" rate entropy (show classical)
  
  putStrLn ""

-- | Example 7: Semantic annealing for optimization
example7_SemanticAnnealing :: IO ()
example7_SemanticAnnealing = do
  putStrLn "=== Example 7: Semantic Annealing ==="
  
  let config = SuperpositionConfig
        { semanticBasis = [Semantic k | k <- [0.0, 0.5 .. 3.0]]
        , coherenceDecay = 0.001
        , entanglementRate = 0.1
        , temperatureParam = 5.0  -- High initial temperature
        }
  
  -- Random initial state
  let components = [(1/sqrt 7 :+ 0, Semantic k) | k <- [0.0, 0.5 .. 3.0]]
      initial = solveSuperposition config components
  
  putStrLn $ "Initial state entropy: " ++ printf "%.4f" (semanticEntanglement initial)
  
  -- Anneal
  let annealed = semanticAnnealing 1.0 config initial
  putStrLn $ "Annealed state entropy: " ++ printf "%.4f" (semanticEntanglement annealed)
  putStrLn $ "Annealed state purity: " ++ printf "%.4f" (purity annealed)
  
  -- Find dominant component (ground state)
  let dominant = findDominantComponents 0.3 annealed
  case dominant of
    [] -> putStrLn "No dominant component found"
    ((amp, Semantic val):_) -> 
      putStrLn $ printf "Ground state: Semantic %.2f with amplitude %.4f" 
                 val (magnitude amp)
  
  putStrLn ""

-- | Run all examples
runAllExamples :: IO ()
runAllExamples = do
  example1_InfoMatterCorrespondence
  example2_MeasurementProblem
  example3_SuperpositionStability
  example4_SemanticEntanglement
  example5_DecoherenceFree
  example6_PhaseTransition
  example7_SemanticAnnealing
  putStrLn "=== All examples completed ===="

-- | Main entry point
main :: IO ()
main = runAllExamples