module CategorySpec where

import Test.Hspec
import InformationalSpacetime.Category
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
    describe "InfoCategory" $ do
        it "creates identity morphisms correctly" $ do
            let obj = InfoObject "test" (Map.singleton "value" 1.0)
            let idMorph = identity obj
            objId (source idMorph) `shouldBe` "test"
            objId (target idMorph) `shouldBe` "test"
            transformation idMorph (objData obj) `shouldBe` objData obj
        
        it "composes morphisms correctly" $ do
            let obj1 = InfoObject "A" (Map.singleton "x" 1.0)
            let obj2 = InfoObject "B" (Map.singleton "x" 2.0)
            let obj3 = InfoObject "C" (Map.singleton "x" 3.0)
            
            let f = InfoMorphism "f" obj1 obj2 (Map.adjust (*2) "x")
            let g = InfoMorphism "g" obj2 obj3 (Map.adjust (+1) "x")
            
            case compose g f of
                Just comp -> do
                    objId (source comp) `shouldBe` "A"
                    objId (target comp) `shouldBe` "C"
                    transformation comp (Map.singleton "x" 1.0) `shouldBe` Map.singleton "x" 3.0
                Nothing -> expectationFailure "Composition should succeed"
        
        it "fails composition with incompatible morphisms" $ do
            let obj1 = InfoObject "A" Map.empty
            let obj2 = InfoObject "B" Map.empty
            let obj3 = InfoObject "C" Map.empty
            
            let f = InfoMorphism "f" obj1 obj2 id
            let g = InfoMorphism "g" obj3 obj1 id
            
            compose f g `shouldBe` Nothing