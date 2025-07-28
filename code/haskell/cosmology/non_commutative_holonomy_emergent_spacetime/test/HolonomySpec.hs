module HolonomySpec where

import Test.Hspec
import InformationalSpacetime.Category
import InformationalSpacetime.Holonomy
import Data.Complex
import Numeric.LinearAlgebra
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "Holonomy" $ do
        it "computes identity holonomy for empty path" $ do
            let cat = InfoCategory [] [] compose identity
            let hol = computeHolonomy [] cat
            rows (holMatrix hol) `shouldBe` 2
            cols (holMatrix hol) `shouldBe` 2
            holPhase hol `shouldBe` (1.0 :+ 0.0)
        
        it "detects non-commutativity in holonomy group" $ do
            let obj1 = InfoObject "A" Map.empty
            let obj2 = InfoObject "B" Map.empty
            let morph1 = InfoMorphism "f" obj1 obj2 id
            let morph2 = InfoMorphism "g" obj2 obj1 id
            
            let cat = InfoCategory [obj1, obj2] [morph1, morph2] compose identity
            let hol1 = computeHolonomy [morph1] cat
            let hol2 = computeHolonomy [morph2] cat
            
            let group = [hol1, hol2]
            isNonCommutative group `shouldBe` False  -- For simple paths
        
        it "computes Berry phase correctly" $ do
            let obj = InfoObject "test" Map.empty
            let morph = InfoMorphism "loop" obj obj id
            let cat = InfoCategory [obj] [morph] compose identity
            let phase = berryPhase [morph] cat
            phase `shouldSatisfy` (\p -> abs p < 1e-10)