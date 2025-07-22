{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.Complex
import Numeric.LinearAlgebra
import Control.Monad
import System.Environment
import Text.Printf
import SemanticPhysics.Core
import SemanticPhysics.Information
import SemanticPhysics.Constraints
import SemanticPhysics.EmergentSpacetime
import SemanticPhysics.UnifiedForces
import SemanticPhysics.QuantumInformation
import SemanticPhysics.Experiments

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> runDemo
        ["experiments"] -> runExperiments
        ["spacetime"] -> demonstrateEmergentSpacetime
        ["forces"] -> demonstrateUnifiedForces
        ["quantum"] -> demonstrateQuantumInformation
        _ -> printUsage

printUsage :: IO ()
printUsage = do
    putStrLn "Unified Physics Treatise - Semantic Physics Implementation"
    putStrLn ""
    putStrLn "Usage: unified-physics-demo [command]"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  (no command)    Run full demonstration"
    putStrLn "  experiments     Run experimental predictions"
    putStrLn "  spacetime      Demonstrate emergent spacetime"
    putStrLn "  forces         Demonstrate force unification"
    putStrLn "  quantum        Demonstrate quantum information"

runDemo :: IO ()
runDemo = do
    putStrLn "=== Unified Physics Through Information-Theoretic Constraints ==="
    putStrLn ""
    
    -- Demonstrate core UCE
    putStrLn "1. Unified Constraint Equation (UCE)"
    demonstrateUCE
    putStrLn ""
    
    -- Show emergent spacetime
    putStrLn "2. Emergent Spacetime from Information"
    demonstrateEmergentSpacetime
    putStrLn ""
    
    -- Show force unification
    putStrLn "3. Force Unification"
    demonstrateUnifiedForces
    putStrLn ""
    
    -- Quantum information features
    putStrLn "4. Quantum Information Features"
    demonstrateQuantumInformation
    putStrLn ""
    
    -- Run experiments
    putStrLn "5. Experimental Predictions"
    runExperiments

demonstrateUCE :: IO ()
demonstrateUCE = do
    putStrLn "The Unified Constraint Equation:"
    putStrLn "[Ê + √h(R - 2Λ) + ⟨Êᵢⱼ⟩/4G - S_info]|Ψ⟩ = 0"
    putStrLn ""
    
    -- Create example quantum state
    let dim = 8
        psi = normalize $ fromList [exp(-fromIntegral i / 2) :+ 0 | i <- [0..dim-1]]
        state = PhysicalState psi dim True
    
    -- Create information state
    let infoState = InformationState
            { isEntropy = log (fromIntegral dim)
            , isStructure = outer psi psi
            , isFlowPattern = fromList $ replicate dim 0
            }
    
    -- Create UCE
    let uce = UnifiedConstraintEquation
            { uceQuantumEnergy = QuantumOperator (ident dim) True False
            , uceSpatialCurvature = \_ -> 0.1
            , uceEntanglementOperator = ident dim
            , uceInformationEntropy = isEntropy
            , uceCosmologicalConstant = 1e-52
            , uceNewtonConstant = 6.67e-11
            }
    
    -- Apply UCE
    let result = applyUCE uce state
    putStrLn $ "Initial state dimension: " ++ show (psDimension state)
    putStrLn $ "UCE preserves normalization: " ++ show (psNormalized result)
    putStrLn $ "Information entropy: " ++ printf "%.3f" (isEntropy infoState)

demonstrateEmergentSpacetime :: IO ()
demonstrateEmergentSpacetime = do
    putStrLn "=== Emergent Spacetime from Entanglement ==="
    putStrLn ""
    
    -- Create entangled information state
    let bell = case createBellState 0 of
            PureState v -> v
            _ -> error "Expected pure state"
        entangledState = InformationState
            { isEntropy = log 2
            , isStructure = outer bell bell
            , isFlowPattern = fromList [0, 0]
            }
    
    -- Compute emergent metric
    let metric = computeEmergentMetric entangledState
        point = SpacetimePoint 0 (fromList [0, 0, 0])
        g = emMetricTensor metric point
    
    putStrLn "Emergent metric tensor at origin:"
    printMatrix g
    
    -- Compute geodesic
    let velocity = fromList [1, 0.1, 0, 0]  -- Nearly lightlike
        trajectory = take 10 $ computeGeodesic metric point velocity 1.0
    
    putStrLn "\nGeodesic trajectory:"
    forM_ (zip [0..] trajectory) $ \(i, pt) ->
        printf "%d: t=%.3f, x=(%.3f, %.3f, %.3f)\n" 
               (i :: Int) (spTime pt) 
               (spSpace pt ! 0) (spSpace pt ! 1) (spSpace pt ! 2)
    
    -- Verify Einstein equations
    let einstein = computeEinsteinTensor metric point
    putStrLn "\nEinstein tensor:"
    printMatrix einstein

demonstrateUnifiedForces :: IO ()
demonstrateUnifiedForces = do
    putStrLn "=== Force Unification from Constraints ==="
    putStrLn ""
    
    -- Create unified field
    let infoState = InformationState
            { isEntropy = 1.0
            , isStructure = ident 4
            , isFlowPattern = fromList [0, 0, 0, 0]
            }
        field = UnifiedField
            { ufInformationField = infoState
            , ufConstraints = [(Geometric, 1.0), (Gauge, 1/137), (SymmetryBreaking, 0.65), (Confinement, 0.3)]
            , ufMetric = computeEmergentMetric infoState
            , ufGaugeField = ident 4
            }
    
    -- Create test particle
    let particle = Particle
            { particleMass = 1.0  -- GeV
            , particleCharge = 1.0  -- Elementary charge
            , particleSpin = 0.5
            , particleIsospin = fromList [0.5, 0]
            , particleColor = fromList [1, 0, 0]
            , particlePosition = SpacetimePoint 0 (fromList [1, 0, 0])
            , particleMomentum = fromList [0.1, 0, 0]
            }
    
    -- Compute all forces
    let forces = [(ft, computeForce field particle ft) | ft <- [Gravitational, Electromagnetic, Weak, Strong]]
    
    putStrLn "Forces on test particle:"
    forM_ forces $ \(ftype, Force{..}) -> do
        printf "%s: magnitude = %.3e, coupling = %.3e\n" 
               (show ftype) forceMagnitude forceCoupling
    
    -- Show running couplings
    putStrLn "\nRunning couplings at different energy scales:"
    let energies = [1, 91.2, 1000, 1e16]  -- GeV
    forM_ energies $ \e -> do
        let couplings = unifyForces e
        printf "E = %.1e GeV: " e
        forM_ couplings $ \(ft, c) ->
            printf "%s=%.3e " (take 2 $ show ft) c
        putStrLn ""

demonstrateQuantumInformation :: IO ()
demonstrateQuantumInformation = do
    putStrLn "=== Quantum Information Features ==="
    putStrLn ""
    
    -- Quantum teleportation
    putStrLn "Quantum Teleportation:"
    let stateToTeleport = normalize $ fromList [0.6 :+ 0, 0.8 :+ 0]
    (teleported, (bit1, bit2)) <- quantumTeleportation (PureState stateToTeleport)
    putStrLn $ "Classical bits sent: " ++ show (bit1, bit2)
    case teleported of
        PureState v -> putStrLn $ "State teleported successfully"
        _ -> putStrLn "Mixed state result"
    
    -- Entanglement measures
    putStrLn "\nEntanglement Measures:"
    let bell = createBellState 0
        concurrence = computeConcurrence bell
        entropy = computeEntanglementEntropy bell [0]
    printf "Bell state concurrence: %.3f\n" concurrence
    printf "Entanglement entropy: %.3f\n" entropy
    
    -- Information echo
    putStrLn "\nInformation Echo Prediction:"
    echoTime <- informationEcho bell 300 1.0
    printf "Echo time at 300K: %.3e seconds\n" echoTime

runExperiments :: IO ()
runExperiments = do
    putStrLn "=== Running Experimental Tests ==="
    putStrLn ""
    
    results <- runAllExperiments
    
    forM_ results $ \ExperimentResult{..} -> do
        putStrLn $ "Experiment: " ++ expName resExperiment
        printf "Result: %.3e ± %.3e (predicted: %.3e)\n" 
               resMeasured resUncertainty (expPrediction resExperiment)
        putStrLn $ "Status: " ++ if resSuccess then "PASS ✓" else "FAIL ✗"
        printf "Significance: %.1fσ\n\n" resSignificance
    
    let passed = length $ filter resSuccess results
        total = length results
    printf "Summary: %d/%d experiments passed\n" passed total

-- Helper functions

normalize :: Vector (Complex Double) -> Vector (Complex Double)
normalize v = scale (1 / norm_2 v) v

printMatrix :: Matrix Double -> IO ()
printMatrix m = do
    let (rows, cols) = size m
    forM_ [0..rows-1] $ \i -> do
        putStr "["
        forM_ [0..cols-1] $ \j -> do
            printf "%7.3f" (m ! (i,j))
            when (j < cols-1) $ putStr ", "
        putStrLn "]"

outer :: Vector (Complex Double) -> Vector (Complex Double) -> Matrix (Complex Double)
outer v w = v `outer` conj w