module Main where

import CAIF.Category.Information
import CAIF.Anholonomy.Field
import CAIF.Geometry.Emergent
import CAIF.Galaxy.Dynamics
import CAIF.Quantum.FieldTheory
import CAIF.Experimental.Predictions
import Text.Printf

main :: IO ()
main = do
  putStrLn "Categorical Anholonomic Information Field (CAIF) Demonstration"
  putStrLn "============================================================\n"
  
  -- Create a model galaxy
  let radius = 5e4  -- 50,000 light years in meters (simplified)
      massDistribution r = 1e40 * (1 - exp(-r/1e4))  -- Exponential disk
      galaxy = createGalaxy radius massDistribution 1000
  
  putStrLn "1. Galaxy Rotation Curve Analysis:"
  putStrLn "Radius (kly) | Newtonian (km/s) | CAIF Total (km/s)"
  putStrLn "-------------------------------------------------"
  
  let radii = [1e3, 5e3, 1e4, 2e4, 3e4, 4e4, 5e4]  -- in light years
  mapM_ (printRotationCurve galaxy) radii
  
  putStrLn "\n2. Information-Rotation Correlation:"
  let correlation = informationRotationCorrelation galaxy
  printf "Correlation coefficient: %.3f\n" correlation
  
  putStrLn "\n3. Testable Predictions:"
  let predictions = generatePredictions galaxy
  printf "- Interferometry phase shift: %.2e radians\n" (interferometryDeviation predictions)
  printf "- GW memory amplitude: %.2e strain\n" (gwMemoryAmplitude predictions)
  printf "- Morphology parameter: %.2f\n" (morphologyParameter predictions)
  
  putStrLn "\n4. Anholonomic Field Properties:"
  let caif = anholonomicField galaxy
      anholonomyMatrix = anholonomyAlgebra caif
  printf "- Field dimension: %d\n" (dimension caif)
  printf "- Anholonomy algebra trace: %.3f\n" (realPart $ trace anholonomyMatrix)
  
  putStrLn "\n5. Cosmological Implications:"
  let vev = vacuumExpectationValue caif
      w = darkEnergyEquationOfState caif
  printf "- Vacuum expectation value: %.2e\n" vev
  printf "- Dark energy equation of state w: %.4f\n" w
  
  putStrLn "\n6. Information Geometry:"
  let infoStateA = InformationState (1.0, 0.0, 0.0)
      infoStateB = InformationState (0.0, 1.0, 0.0)
      distance = informationDistance id id infoStateA infoStateB
  printf "- Information distance between orthogonal states: %.3f\n" distance
  
  putStrLn "\nDemonstration complete."

printRotationCurve :: Galaxy -> Double -> IO ()
printRotationCurve galaxy r = do
  let g = 6.67430e-11
      m_r = baryonicMass galaxy r
      v_newton = sqrt (g * m_r / r) / 1000  -- Convert to km/s
      v_total = flatRotationCurve galaxy r / 1000  -- Convert to km/s
  printf "%12.1f | %16.1f | %17.1f\n" (r/1e3) v_newton v_total