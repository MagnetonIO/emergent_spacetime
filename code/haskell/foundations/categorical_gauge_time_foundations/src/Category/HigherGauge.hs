{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Category.HigherGauge where

import Category.Base
import Data.Kind (Type)
import qualified Data.Set as Set

data TwoGaugeCategory where
    TwoGauge :: 
        { twoConfigurations :: Set.Set Configuration
        , oneGaugeTransformations :: Configuration -> Configuration -> Set.Set GaugeTransformation
        , twoGaugeTransformations :: GaugeTransformation -> GaugeTransformation -> Set.Set GaugeOfGauge
        , twoVerticalCompose :: GaugeOfGauge -> GaugeOfGauge -> GaugeOfGauge
        , twoHorizontalCompose :: GaugeOfGauge -> GaugeOfGauge -> GaugeOfGauge
        } -> TwoGaugeCategory

data Configuration = Config String deriving (Eq, Ord, Show)
data GaugeTransformation = GaugeTrans String deriving (Eq, Ord, Show)
data GaugeOfGauge = GaugeOfGauge String deriving (Eq, Ord, Show)

coherenceCondition :: TwoGaugeCategory -> Bool
coherenceCondition twoGauge =
    let configs = Set.toList $ twoConfigurations twoGauge
    in all (checkCoherence twoGauge) [(c1, c2) | c1 <- configs, c2 <- configs]
  where
    checkCoherence _ _ = True

data SpinNetwork = SpinNetwork
    { snNodes :: Set.Set Node
    , snEdges :: Set.Set Edge
    , snSpins :: Edge -> Spin
    , snIntertwiners :: Node -> Intertwiner
    }

data Node = Node Int deriving (Eq, Ord, Show)
data Edge = Edge Node Node deriving (Eq, Ord, Show)
type Spin = Double
type Intertwiner = [Double]

data PachnerMove 
    = Pachner23 SpinNetwork SpinNetwork
    | Pachner32 SpinNetwork SpinNetwork
    | Pachner14 SpinNetwork SpinNetwork
    | Pachner41 SpinNetwork SpinNetwork
    deriving (Show)

applyPachnerMove :: PachnerMove -> SpinNetwork -> SpinNetwork
applyPachnerMove (Pachner23 _ target) _ = target
applyPachnerMove (Pachner32 _ target) _ = target
applyPachnerMove (Pachner14 _ target) _ = target
applyPachnerMove (Pachner41 _ target) _ = target

spinFoam :: [SpinNetwork] -> [(SpinNetwork, SpinNetwork, PachnerMove)]
spinFoam networks = 
    [(n1, n2, Pachner23 n1 n2) | n1 <- networks, n2 <- networks, n1 /= n2]

data PresheafOnGauge a where
    Presheaf :: 
        { presheafObject :: Configuration -> a
        , presheafMorphism :: GaugeTransformation -> (a -> a)
        , presheafFunctoriality :: Bool
        } -> PresheafOnGauge a

sheafCondition :: PresheafOnGauge a -> Bool
sheafCondition _ = True

data GaugeTopos = GaugeTopos
    { toposSite :: TwoGaugeCategory
    , toposSheaves :: Set.Set (PresheafOnGauge ())
    , toposSubobjectClassifier :: PresheafOnGauge Bool
    }

constructGaugeTopos :: TwoGaugeCategory -> GaugeTopos
constructGaugeTopos cat = GaugeTopos
    { toposSite = cat
    , toposSheaves = Set.empty
    , toposSubobjectClassifier = Presheaf (const True) (const id) True
    }

data StackOnGauge = StackOnGauge
    { stackObjects :: Set.Set Configuration
    , stackMorphisms :: Configuration -> Configuration -> Set.Set GaugeTransformation
    , stackDescent :: Bool
    }

descentCondition :: StackOnGauge -> Bool
descentCondition stack = stackDescent stack