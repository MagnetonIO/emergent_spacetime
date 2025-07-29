import Test.Hspec
import Test.QuickCheck
import CAIF.Category.Information
import CAIF.Anholonomy.Field
import CAIF.Galaxy.Dynamics
import Data.Complex
import Numeric.LinearAlgebra

main :: IO ()
main = hspec $ do
  describe "Information Category" $ do
    it "computes information distance symmetrically" $ do
      let stateA = InformationState (1.0 :: Double)
          stateB = InformationState (2.0 :: Double)
          embed = \x -> [x, x^2]
          dist1 = informationDistance embed embed stateA stateB
          dist2 = informationDistance embed embed stateB stateA
      abs (dist1 - dist2) `shouldSatisfy` (< 0.01)
    
    it "Berry phase is non-zero for non-trivial loops" $ do
      let transform1 = InfoTransform id 0.5
          transform2 = InfoTransform (*2) 0.7
          transform3 = InfoTransform (/2) 0.6
          phase = berryPhase [transform1, transform2, transform3]
      phase `shouldSatisfy` (/= 0)
  
  describe "Anholonomic Field" $ do
    it "generates non-commutative algebra" $ do
      let caif = generateCAIF 10 1e4
          algebra = anholonomyAlgebra caif
      -- Check that algebra is not diagonal (non-commutative)
      let offDiagonal = sum [abs (algebra ! i ! j) | i <- [0..3], j <- [0..3], i /= j]
      offDiagonal `shouldSatisfy` (> 0)
    
    it "produces flat rotation curves" $ do
      let galaxy = createGalaxy 5e4 (\r -> 1e40 * (1 - exp(-r/1e4))) 100
          v1 = flatRotationCurve galaxy 1e4
          v2 = flatRotationCurve galaxy 3e4
          v3 = flatRotationCurve galaxy 5e4
          -- Check that velocities are approximately constant (flat)
          relDiff v1 v2 = abs (v1 - v2) / max v1 v2
      relDiff v2 v3 `shouldSatisfy` (< 0.2)  -- Within 20%
  
  describe "Experimental Predictions" $ do
    it "interferometry phase shift scales with enclosed mass" $ do
      let exp1 = InterferometryExperiment 1000 1e30 1e6
          exp2 = InterferometryExperiment 1000 2e30 1e6
          caif = generateCAIF 10 1e4
          phase1 = quantumInterferometryPhaseShift exp1 caif
          phase2 = quantumInterferometryPhaseShift exp2 caif
      phase2 `shouldSatisfy` (> phase1)
    
    it "dark energy equation of state near -1" $ do
      let caif = generateCAIF 50 1e5
          w = darkEnergyEquationOfState caif
      abs (w + 1) `shouldSatisfy` (< 0.5)  -- w should be close to -1

  where
    (!) m i j = m `atIndex` (i, j)