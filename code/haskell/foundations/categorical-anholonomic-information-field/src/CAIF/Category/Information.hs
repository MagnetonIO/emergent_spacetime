{-# LANGUAGE TypeOperators #-}
module CAIF.Category.Information where

import Control.Category
import Control.Monad
import Data.Functor.Identity
import Data.Profunctor
import Prelude hiding (id, (.))

-- | Information states as objects in our category
newtype InformationState a = InformationState { unInfoState :: a }
  deriving (Show, Eq, Ord, Functor)

-- | Information transformations as 1-morphisms
data InfoTransform a b = InfoTransform
  { transform :: a -> b
  , entropy :: Double  -- Shannon entropy of the transformation
  }

-- | 2-morphisms as homotopies between transformations
data Homotopy f g where
  Homotopy :: (forall x. f x -> g x) -> Homotopy f g

-- | The 2-category of information
data InfoCategory a b where
  IdInfo :: InfoCategory a a
  CompInfo :: InfoCategory b c -> InfoCategory a b -> InfoCategory a c
  Transform :: InfoTransform a b -> InfoCategory a b

instance Category InfoCategory where
  id = IdInfo
  (.) = CompInfo

-- | Calculate information distance between states
informationDistance :: (a -> [Double]) -> (b -> [Double]) 
                    -> InformationState a -> InformationState b 
                    -> Double
informationDistance embedA embedB (InformationState a) (InformationState b) =
  let va = embedA a
      vb = embedB b
      -- Using negative log of morphism count approximation
      morphismCount = 1.0 / (1.0 + sum (zipWith (\x y -> (x - y)^2) va vb))
  in -log morphismCount + entropyTerm va vb
  where
    entropyTerm va vb = 
      let p = normalize va
          q = normalize vb
      in kullbackLeibler p q
    
    normalize xs = map (/ sum xs) xs
    
    kullbackLeibler p q = sum $ zipWith (\pi qi -> if pi > 0 then pi * log (pi / qi) else 0) p q

-- | Parallel transport functor for information states
data ParallelTransport m a b = ParallelTransport
  { transportMap :: m a -> m b
  , phase :: Double  -- Anholonomic phase accumulated
  }

-- | Berry phase calculation for closed loops
berryPhase :: [InfoTransform a a] -> Double
berryPhase transforms = 
  let totalEntropy = sum $ map entropy transforms
      pathLength = fromIntegral $ length transforms
  in totalEntropy / pathLength * 2 * pi