module Physics.Functorial.CoreSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Complex
import qualified Numeric.LinearAlgebra as LA
import Physics.Functorial.Core

tests :: TestTree
tests = testGroup "Core Tests"
  [ testCase "von Neumann entropy of pure state" testPureStateEntropy
  , testCase "von Neumann entropy of maximally mixed state" testMaximallyMixedEntropy
  , testProperty "information content scaling" prop_informationScaling
  , testCase "information-energy correspondence" testInfoEnergyCorrespondence
  ]

testPureStateEntropy :: Assertion
testPureStateEntropy = do
  let pureState = InformationState $ LA.fromLists [[1, 0], [0, 0]]
      entropy = vonNeumannEntropy pureState
  entropy @?= 0.0

testMaximallyMixedEntropy :: Assertion  
testMaximallyMixedEntropy = do
  let dim = 2
      mixedState = InformationState $ LA.scale (1 / fromIntegral dim) (LA.ident dim)
      entropy = vonNeumannEntropy mixedState
      expected = log (fromIntegral dim)
  abs (entropy - expected) < 1e-10 @? "Entropy should be log(dim)"

prop_informationScaling :: Positive Double -> Positive Double -> Bool
prop_informationScaling (Positive e) (Positive l) =
  let a = 1.0
      info1 = informationContent e l a
      info2 = informationContent (2*e) l a
      ratio = info2 / info1
      expected = 2 ** 0.75
  in abs (ratio - expected) < 1e-10

testInfoEnergyCorrespondence :: Assertion
testInfoEnergyCorrespondence = do
  let energy = 1e19  -- Planck scale
      length = 1e-35  -- Planck length
      a = 1.0
      info = informationContent energy length a
  info > 0 @? "Information content should be positive"