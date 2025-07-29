{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module CategoricalAnholonomic.Core where

import Data.Kind (Type)
import Control.Monad
import Control.Applicative

class Category cat where
    type Obj cat :: Type -> Type
    type Mor cat :: Type -> Type -> Type
    
    id :: Obj cat a -> Mor cat a a
    compose :: Mor cat b c -> Mor cat a b -> Mor cat a c
    
    source :: Mor cat a b -> Obj cat a
    target :: Mor cat a b -> Obj cat b

class Category cat => MonoidalCategory cat where
    type Tensor cat :: Type -> Type -> Type
    type Unit cat :: Type
    
    tensor :: Mor cat a b -> Mor cat c d -> Mor cat (Tensor cat a c) (Tensor cat b d)
    unit :: Obj cat (Unit cat)
    
    associator :: Obj cat a -> Obj cat b -> Obj cat c -> 
                  Mor cat (Tensor cat (Tensor cat a b) c) (Tensor cat a (Tensor cat b c))
    leftUnitor :: Obj cat a -> Mor cat (Tensor cat (Unit cat) a) a
    rightUnitor :: Obj cat a -> Mor cat (Tensor cat a (Unit cat)) a

data Functor cat1 cat2 = Functor
    { mapObj :: forall a. Obj cat1 a -> Obj cat2 (FMap cat1 cat2 a)
    , mapMor :: forall a b. Mor cat1 a b -> Mor cat2 (FMap cat1 cat2 a) (FMap cat1 cat2 b)
    }

type family FMap (cat1 :: Type) (cat2 :: Type) :: Type -> Type

data NatTrans cat1 cat2 f g = NatTrans
    { component :: forall a. Obj cat1 a -> Mor cat2 (FMap cat1 cat2 a) (FMap cat1 cat2 a)
    }

class (MonoidalCategory cat) => BraidedCategory cat where
    braiding :: Obj cat a -> Obj cat b -> Mor cat (Tensor cat a b) (Tensor cat b a)

class BraidedCategory cat => SymmetricCategory cat where
    symmetry :: Obj cat a -> Obj cat b -> 
                compose (braiding b a) (braiding a b) ~ id (tensor a b)

data Path a b where
    PathId :: a -> Path a a
    PathCompose :: Path b c -> Path a b -> Path a c
    PathInverse :: Path a b -> Path b a

pathCategory :: Category Path where
    type Obj Path = Identity
    type Mor Path = Path
    
    id (Identity x) = PathId x
    compose = PathCompose
    source (PathId x) = Identity x
    source (PathCompose p _) = source p
    source (PathInverse p) = target p
    target (PathId x) = Identity x
    target (PathCompose _ p) = target p
    target (PathInverse p) = source p

newtype Identity a = Identity a

instance Category Path where
    type Obj Path = Identity
    type Mor Path = Path