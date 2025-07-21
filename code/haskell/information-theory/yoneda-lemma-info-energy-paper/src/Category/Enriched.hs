{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Category.Enriched where

import Control.Category
import Data.Functor.Identity
import Data.Profunctor
import Prelude hiding (id, (.))

class Category v => EnrichedCategory v ob hom | hom -> v ob where
  eid :: ob a -> v (Identity ()) (hom a a)
  ecomp :: (ob a, ob b, ob c) => v (hom b c, hom a b) (hom a c)

data EnrichedFunctor v c d f = EnrichedFunctor
  { efmap_ob :: forall a. c a -> d (f a)
  , efmap_hom :: forall a b. (c a, c b) => v (Hom c a b) (Hom d (f a) (f b))
  }

type Hom c a b = c a b

data YonedaEmbedding v c a b where
  YonedaEmb :: (forall x. c x -> v (Hom c x a) (Hom v (Hom c x b) (Identity ())))
            -> YonedaEmbedding v c a b

enrichedYonedaLemma :: (EnrichedCategory v ob c) 
                    => (forall x. ob x -> v (c x a) (f x))
                    -> v (Identity ()) (f a)
enrichedYonedaLemma nat = nat eid