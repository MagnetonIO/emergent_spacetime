{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Physics.Functorial.Core
  ( -- * Core Types
    InformationState(..)
  , PhysicalState(..)
  , InformationMorphism(..)
  , PhysicalMorphism(..)
  
    -- * Categories
  , InfoCategory(..)
  , PhysCategory(..)
  
    -- * Functors
  , FundamentalFunctor(..)
  , applyFundamentalFunctor
  
    -- * Entropy
  , vonNeumannEntropy
  , informationContent
  ) where

import Control.Category
import Control.Monad
import Data.Complex
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Prelude hiding (id, (.))

-- | Information state represented as density operator
newtype InformationState = InformationState 
  { densityMatrix :: LA.Matrix (Complex Double) 
  } deriving (Show, Eq)

-- | Physical state in Hilbert space
newtype PhysicalState = PhysicalState 
  { stateVector :: LA.Vector (Complex Double) 
  } deriving (Show, Eq)

-- | Information-preserving morphism
data InformationMorphism = InformationMorphism
  { infoTransform :: InformationState -> InformationState
  , entropyBound :: Double  -- Upper bound on entropy increase
  }

-- | Unitary morphism in physical space
data PhysicalMorphism = PhysicalMorphism
  { unitary :: LA.Matrix (Complex Double)
  , phaseShift :: Double
  }

-- | Category of information states
class Category cat => InfoCategory cat where
  type InfoObj cat :: *
  type InfoMorph cat :: * -> * -> *
  
  entropy :: InfoObj cat -> Double
  compose :: InfoMorph cat b c -> InfoMorph cat a b -> InfoMorph cat a c
  identity :: InfoObj cat -> InfoMorph cat a a

-- | Category of physical states  
class Category cat => PhysCategory cat where
  type PhysObj cat :: *
  type PhysMorph cat :: * -> * -> *
  
  hilbertDim :: PhysObj cat -> Int
  evolve :: PhysMorph cat a b -> PhysObj cat -> PhysObj cat

-- | The fundamental functor F: Info → Phys
data FundamentalFunctor = FundamentalFunctor
  { mapObject :: InformationState -> PhysicalState
  , mapMorphism :: InformationMorphism -> PhysicalMorphism
  , preservesStructure :: Bool
  }

-- | Apply the fundamental functor
applyFundamentalFunctor :: FundamentalFunctor -> InformationState -> PhysicalState
applyFundamentalFunctor f = mapObject f

-- | Calculate von Neumann entropy
vonNeumannEntropy :: InformationState -> Double
vonNeumannEntropy (InformationState rho) = 
  let eigenvals = LA.toList $ LA.eigenvaluesSH rho
      realParts = map realPart eigenvals
      -- Filter out numerical zeros and negatives
      validEigenvals = filter (> 1e-10) realParts
  in -sum [p * log p | p <- validEigenvals]

-- | Information content following the paper's scaling law
informationContent :: Double -> Double -> Double -> Double
informationContent energy length a = a * (energy ** 0.75) * (length ** 2)