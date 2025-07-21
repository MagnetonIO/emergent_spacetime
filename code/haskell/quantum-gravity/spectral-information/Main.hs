{-# LANGUAGE RecordWildCards #-}

module Main where

import SpectralTypes
import InformationOperator
import SpectralSolver
import ErrorCorrection
import ScatteringTheory
import Numeric.LinearAlgebra hiding (Complex)
import qualified Numeric.LinearAlgebra as LA
import Data.Complex
import Text.Printf

main :: IO ()
main = do
  putStrLn "=== Spectral Theory of Information-Energy Operators ==="
  putStrLn "=== Demonstration of Key Results from the Paper ==="
  putStrLn ""
  
  -- Example 1: Information density operator spectrum
  example1_informationSpectrum
  
  -- Example 2: Information-energy correspondence
  example2_energyCorrespondence
  
  -- Example 3: Spacetime stability analysis
  example3_spacetimeStability
  
  -- Example 4: Scattering calculations
  example4_scatteringTheory
  
  -- Example 5: Error correction and black holes
  example5_errorCorrection

-- | Example 1: Calculate spectrum of information density operator
example1_informationSpectrum :: IO ()
example1_informationSpectrum = do
  putStrLn "\n=== Example 1: Information Density Operator Spectrum ==="
  
  let gridPoints = 100
      -- Create potential well (representing localized information)
      field = createPotentialWell gridPoints 5.0 10.0
      op = applyBoundaryConditions $ 
           constructInformationOperator field gridPoints Dirichlet
      
      -- Solve for spectrum
      spectral = solveSpectrum op 10
      
  putStrLn $ "Grid points: " ++ show gridPoints
  putStrLn $ "Operator dimension: " ++ show (rows (operatorMatrix op)) ++ "x" ++ show (cols (operatorMatrix op))
  putStrLn $ "Is Hermitian: " ++ show (isHermitian op)
  putStrLn $ "Threshold: " ++ printf "%.4f" (threshold op)
  
  putStrLn "\nDiscrete spectrum (bound states):"
  mapM_ (printEigenvalue . (,) "  ") (take 5 $ discreteEigenvalues spectral)
  
  case findSpectralGap spectral of
    Just gap -> putStrLn $ "\nSpectral gap: " ++ printf "%.4f" gap
    Nothing -> putStrLn "\nNo spectral gap found"

-- | Example 2: Demonstrate information-energy correspondence
example2_energyCorrespondence :: IO ()
example2_energyCorrespondence = do
  putStrLn "\n=== Example 2: Information-Energy Correspondence ==="
  putStrLn "E = c²ε + O(ε²/M_P c²)"
  
  let epsilons = [0.1, 0.5, 1.0, 2.0, 5.0] :: [Double]
      
  putStrLn "\nε (info)  →  E (energy)"
  putStrLn "---------------------"
  forM_ epsilons $ \eps -> do
    let info = InformationDensity eps
        energy = informationToEnergy info
        Energy e = energy
    printf "%.1f      →  %.3f\n" eps e
  
  -- Semiclassical eigenvalues
  putStrLn "\nSemiclassical spectrum (n = 0,1,2,...):"
  forM_ [0..4] $ \n -> do
    let InformationDensity eps_n = semiclassicalEigenvalue n
    printf "ε_%d = %.4f\n" n eps_n

-- | Example 3: Check spacetime stability conditions
example3_spacetimeStability :: IO ()
example3_spacetimeStability = do
  putStrLn "\n=== Example 3: Spacetime Stability Analysis ==="
  
  let field = createHarmonicField 50 1.0
      op = constructInformationOperator field 50 Dirichlet
      spectral = solveSpectrum op 20
      
      errorParams = ErrorCorrectionParams
        { syndromeRate = 1000.0
        , recoveryEnergy = Energy 0.01
        , decoherenceRate = 0.1
        , correctionTime = 0.001
        }
      
      stable = checkSpacetimeStability spectral errorParams
      InformationDensity critThresh = criticalThreshold errorParams
      
  putStrLn $ "Decoherence rate: " ++ show (decoherenceRate errorParams)
  putStrLn $ "Correction time: " ++ show (correctionTime errorParams)
  putStrLn $ "Critical threshold: " ++ printf "%.4f" critThresh
  putStrLn $ "Continuous threshold: " ++ printf "%.4f" (continuousThreshold spectral)
  putStrLn $ "\nSpacetime stable: " ++ show stable
  
  -- Check holographic bound
  let testDensity = InformationDensity 1.0
      radius = 10.0
      holographic = checkHolographicBound testDensity radius
  putStrLn $ "\nHolographic bound satisfied (r=" ++ show radius ++ "): " ++ show holographic

-- | Example 4: Information scattering calculations  
example4_scatteringTheory :: IO ()
example4_scatteringTheory = do
  putStrLn "\n=== Example 4: Information Scattering Theory ==="
  
  let field = createBarrier 100 2.0 5.0
      op = constructInformationOperator field 100 Outgoing
      energy = 3.0  -- Above barrier
      
  putStrLn $ "Scattering from barrier potential"
  putStrLn $ "Barrier height: 2.0, width: 5.0"
  putStrLn $ "Incident energy: " ++ show energy
  
  -- Calculate scattering amplitudes at different angles
  putStrLn "\nAngle    |f(θ)|²    σ(θ)"
  putStrLn "------------------------"
  forM_ [0, pi/6, pi/3, pi/2, 2*pi/3, 5*pi/6, pi] $ \theta -> do
    let amp = calculateScatteringAmplitude op energy theta
        diffCS = differentialCrossSection amp
    printf "%.2f     %.4f     %.4f\n" theta (magnitude (amplitude amp)^2) diffCS
  
  -- Total cross section
  let forwardAmp = calculateScatteringAmplitude op energy 0
      totalCS = totalCrossSection forwardAmp
  putStrLn $ "\nTotal cross-section: " ++ printf "%.4f" totalCS
  
  -- Verify optical theorem
  let opticalOK = verifyOpticalTheorem forwardAmp
  putStrLn $ "Optical theorem satisfied: " ++ show opticalOK
  
  -- Phase shifts
  putStrLn "\nPhase shifts δ_l:"
  forM_ [0..3] $ \l -> do
    let delta = phaseShift op energy l
    printf "δ_%d = %.4f\n" l delta

-- | Example 5: Error correction and black hole entropy
example5_errorCorrection :: IO ()
example5_errorCorrection = do
  putStrLn "\n=== Example 5: Error Correction and Black Hole Entropy ==="
  
  -- Simple repetition code
  let code = repetitionCode 3
  putStrLn $ "Quantum error correcting code:"
  putStrLn $ "  Physical qubits: " ++ show (physicalQubits code)
  putStrLn $ "  Logical qubits: " ++ show (logicalQubits code)
  putStrLn $ "  Code distance: " ++ show (codeDistance code)
  putStrLn $ "  Code rate: " ++ printf "%.3f" (codeRate code)
  
  -- Black hole entropy
  putStrLn "\nBlack hole entropy (Bekenstein-Hawking):"
  putStrLn "S = A/(4ℓ_P²) = ∫ ε(x) d³x"
  
  forM_ [1.0, 2.0, 5.0, 10.0] $ \r -> do
    let density = InformationDensity 0.1
        entropy = blackHoleEntropy density r
    printf "  Radius = %.1f: S = %.2f\n" r entropy
  
  -- Modified dispersion relation
  putStrLn "\nModified dispersion relation:"
  putStrLn "E² = (pc)² + (mc²)² + ε_correction(p/M_P c)"
  
  let m = 1.0  -- mass
      dispersion p = 
        let e0 = sqrt ((p * speedOfLight)^2 + (m * speedOfLight^2)^2)
            correction = 0.01 * (p / planckMass / speedOfLight)^2
        in e0 + correction
        
  putStrLn "\np/M_P c    E/mc²"
  putStrLn "----------------"
  forM_ [0, 0.1, 0.5, 1.0] $ \ratio -> do
    let p = ratio * planckMass * speedOfLight
        e = dispersion p
    printf "%.1f       %.4f\n" ratio (e / (m * speedOfLight^2))

-- | Helper function to print eigenvalues
printEigenvalue :: (String, Eigenvalue) -> IO ()
printEigenvalue (prefix, RealEigenvalue e) = 
  putStrLn $ prefix ++ printf "ε = %.6f" e
printEigenvalue (prefix, ComplexEigenvalue c) = 
  putStrLn $ prefix ++ printf "ε = %.4f - i%.4f (resonance)" (realPart c) (decayWidth c / 2)