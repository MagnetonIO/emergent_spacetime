{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Category.Base where

import Data.Kind (Type)
import qualified Data.Set as Set

class Category cat where
    type Ob cat :: Type
    type Mor cat :: Type -> Type -> Type
    
    idMor :: Ob cat ~ a => Mor cat a a
    compose :: Mor cat b c -> Mor cat a b -> Mor cat a c
    
    source :: Mor cat a b -> a
    target :: Mor cat a b -> b

data Functor cat1 cat2 where
    Functor :: (Category cat1, Category cat2) =>
        { objectMap :: Ob cat1 -> Ob cat2
        , morphismMap :: forall a b. Mor cat1 a b -> Mor cat2 (objectMap a) (objectMap b)
        } -> Functor cat1 cat2

composeFunctor :: Functor cat2 cat3 -> Functor cat1 cat2 -> Functor cat1 cat3
composeFunctor (Functor obMap2 morMap2) (Functor obMap1 morMap1) =
    Functor (obMap2 . obMap1) (\f -> morMap2 (morMap1 f))

data NaturalTransformation f g where
    NatTrans :: (Functor cat1 cat2 ~ f, Functor cat1 cat2 ~ g) =>
        { components :: forall a. Ob cat1 ~ a => Mor cat2 (objectMap f a) (objectMap g a)
        } -> NaturalTransformation f g

class Category cat => Groupoid cat where
    inverse :: Mor cat a b -> Mor cat b a
    
    inverseLaw1 :: Mor cat a b -> compose f (inverse f) == idMor
    inverseLaw2 :: Mor cat a b -> compose (inverse f) f == idMor

data TwoCategory where
    TwoCategory :: 
        { objects :: Set.Set obj
        , oneMorphisms :: obj -> obj -> Set.Set mor1
        , twoMorphisms :: mor1 -> mor1 -> Set.Set mor2
        , verticalCompose :: mor2 -> mor2 -> mor2
        , horizontalCompose :: mor2 -> mor2 -> mor2
        } -> TwoCategory

data Topos cat where
    Topos :: Category cat =>
        { subobjectClassifier :: Ob cat
        , truthArrow :: Mor cat () subobjectClassifier
        , pullback :: forall a b c. Mor cat a c -> Mor cat b c -> 
                     (Ob cat, Mor cat pullback a, Mor cat pullback b)
        } -> Topos cat