module SemanticPhysics.CoreSpec (coreSpec) where

import Test.Hspec
import Test.QuickCheck
import Data.Complex
import Numeric.LinearAlgebra
import SemanticPhysics.Core

coreSpec :: Spec
coreSpec = describe "SemanticPhysics.Core" $ do
    describe "PhysicalState" $ do
        it "maintains normalization" $ do
            let psi = fromList [1, 0, 0, 0] :: Vector (Complex Double)
                state = PhysicalState psi 4 True
            psNormalized state `shouldBe` True
            norm_2 (psAmplitudes state) `shouldSatisfy` (\x -> abs (x - 1.0) < 1e-10)
    
    describe "MetricTensor" $ do
        it "has correct signature for Lorentzian metric" $ do
            let metric = MetricTensor (ident 4) (-1, 3) 4
            mtSignature metric `shouldBe` (-1, 3)
    
    describe "computeMetricFromEntanglement" $ do
        it "produces symmetric metric" $ do
            let infoState = InformationState 1.0 (ident 4) (fromList [0,0,0,0])
                metric = computeMetricFromEntanglement infoState
                g = mtComponents metric
            norm_F (g - tr g) `shouldSatisfy` (< 1e-10)