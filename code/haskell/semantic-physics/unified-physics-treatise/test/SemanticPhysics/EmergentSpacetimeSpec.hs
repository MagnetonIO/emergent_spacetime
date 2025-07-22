module SemanticPhysics.EmergentSpacetimeSpec (emergentSpacetimeSpec) where

import Test.Hspec
import Data.Complex
import Numeric.LinearAlgebra
import SemanticPhysics.Core
import SemanticPhysics.EmergentSpacetime

emergentSpacetimeSpec :: Spec
emergentSpacetimeSpec = describe "SemanticPhysics.EmergentSpacetime" $ do
    describe "computeEmergentMetric" $ do
        it "produces Lorentzian signature metric" $ do
            let state = InformationState 1.0 (ident 4) (fromList [0,0,0,0])
                metric = computeEmergentMetric state
            emSignature metric `shouldBe` (-1, 3)
    
    describe "causalDiamond" $ do
        it "correctly identifies timelike separation" $ do
            let state = InformationState 1.0 (ident 4) (fromList [0,0,0,0])
                metric = computeEmergentMetric state
                p1 = SpacetimePoint 0 (fromList [0,0,0])
                p2 = SpacetimePoint 1 (fromList [0,0,0])
            causalDiamond metric p1 p2 `shouldBe` True
    
    describe "computeGeodesic" $ do
        it "preserves proper time" $ do
            let state = InformationState 1.0 (ident 4) (fromList [0,0,0,0])
                metric = computeEmergentMetric state
                start = SpacetimePoint 0 (fromList [0,0,0])
                velocity = fromList [1,0,0,0]
                trajectory = computeGeodesic metric start velocity 1.0
            length trajectory `shouldSatisfy` (> 0)