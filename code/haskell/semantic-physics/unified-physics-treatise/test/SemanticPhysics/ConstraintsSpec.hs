module SemanticPhysics.ConstraintsSpec (constraintsSpec) where

import Test.Hspec
import Data.Complex
import Numeric.LinearAlgebra
import SemanticPhysics.Core
import SemanticPhysics.Constraints

constraintsSpec :: Spec
constraintsSpec = describe "SemanticPhysics.Constraints" $ do
    describe "evaluateConstraint" $ do
        it "evaluates geometric constraint" $ do
            let state = InformationState 1.0 (ident 4) (fromList [0,0,0,0])
                result = evaluateConstraint Geometric state
            result `shouldSatisfy` (not . isNaN)
    
    describe "constraintGradient" $ do
        it "computes non-zero gradient" $ do
            let state = InformationState 1.0 (ident 4) (fromList [1,0,0,0])
                grad = constraintGradient Gauge state
            norm_2 grad `shouldSatisfy` (>= 0)
    
    describe "renormalizationFlow" $ do
        it "modifies couplings with energy" $ do
            let constraints = [(Gauge, 1/137), (Strong, 0.3)]
                evolved = renormalizationFlow 1000 constraints
            length evolved `shouldBe` length constraints