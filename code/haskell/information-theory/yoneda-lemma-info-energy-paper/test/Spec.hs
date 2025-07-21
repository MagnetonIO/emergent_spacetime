module Main where

import Test.Hspec
import Test.QuickCheck
import InfoEnergy.Core
import InfoEnergy.Quantum
import InfoEnergy.BlackHole

main :: IO ()
main = hspec $ do
  describe "Core Information Theory" $ do
    it "entropy is non-negative" $ do
      property $ \ps -> 
        let probs = map (Prob . abs) ps
            totalProb = sum $ map unProb probs
            normalizedProbs = if totalProb > 0 
                             then map (\(Prob p) -> Prob (p / totalProb)) probs
                             else [Prob 1.0]
        in unEntropy (entropy normalizedProbs) >= 0
    
    it "Landauer principle gives positive energy" $ do
      property $ \dh t -> 
        t > 0 ==> unEnergy (landauerPrinciple (Entropy (abs dh)) (Temp t)) >= 0
  
  describe "Quantum Information" $ do
    it "von Neumann entropy is non-negative" $ do
      let pureState = Pure undefined
      pending
    
    it "mixed states have higher entropy than pure states" $ do
      pending
  
  describe "Black Hole Physics" $ do
    it "Bekenstein-Hawking entropy increases with area" $ do
      property $ \a1 a2 ->
        a1 > 0 && a2 > 0 && a1 < a2 ==> 
        unEntropy (bekensteinHawkingEntropy (Area a1)) < 
        unEntropy (bekensteinHawkingEntropy (Area a2))
    
    it "Hawking temperature decreases with mass" $ do
      property $ \m1 m2 ->
        m1 > 0 && m2 > 0 && m1 < m2 ==>
        unTemp (hawkingTemperature (Mass m1)) > 
        unTemp (hawkingTemperature (Mass m2))