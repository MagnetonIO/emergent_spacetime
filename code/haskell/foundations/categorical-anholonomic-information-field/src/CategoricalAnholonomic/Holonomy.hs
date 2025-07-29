{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module CategoricalAnholonomic.Holonomy where

import CategoricalAnholonomic.Core
import CategoricalAnholonomic.InfoCategory
import Data.Complex
import qualified Data.Vector as V
import Numeric.LinearAlgebra hiding (Vector)
import Control.Monad.State

data Loop a = Loop 
    { loopBase :: a
    , loopPath :: [a]
    , loopClosed :: Bool
    }

composeLoops :: Eq a => Loop a -> Loop a -> Maybe (Loop a)
composeLoops l1 l2 
    | loopBase l1 == loopBase l2 && loopClosed l1 && loopClosed l2 =
        Just $ Loop (loopBase l1) (loopPath l1 ++ loopPath l2) True
    | otherwise = Nothing

data HolonomyOperator = HolonomyOperator
    { holMatrix :: Matrix (Complex Double)
    , holPhase :: Complex Double
    , holNonAbelian :: Bool
    }

holonomyFunctor :: Functor Path InfoCat
holonomyFunctor = Functor
    { mapObj = \(Identity x) -> InfoState (V.singleton 1)
    , mapMor = pathToInfo
    }
  where
    pathToInfo :: Path a b -> InfoTransform (FMap Path InfoCat a) (FMap Path InfoCat b)
    pathToInfo (PathId _) = InfoTransform (ident 1)
    pathToInfo (PathCompose p1 p2) = 
        let InfoTransform m1 = pathToInfo p1
            InfoTransform m2 = pathToInfo p2
        in InfoTransform (m1 <> m2)
    pathToInfo (PathInverse p) = 
        let InfoTransform m = pathToInfo p
        in InfoTransform (inv m)

parallelTransport :: Loop a -> HolonomyOperator
parallelTransport loop = HolonomyOperator
    { holMatrix = computeHolonomy (loopPath loop)
    , holPhase = computePhase (loopPath loop)
    , holNonAbelian = True
    }
  where
    computeHolonomy :: [a] -> Matrix (Complex Double)
    computeHolonomy [] = ident 2
    computeHolonomy (x:xs) = localConnection x <> computeHolonomy xs
    
    localConnection :: a -> Matrix (Complex Double)
    localConnection _ = (2><2) [1, 0.1 :+ 0.1, 0.1 :+ (-0.1), 1]
    
    computePhase :: [a] -> Complex Double
    computePhase path = exp (0 :+ (0.1 * fromIntegral (length path)))

wilsonLoop :: Loop a -> Complex Double
wilsonLoop loop = 
    let HolonomyOperator mat _ _ = parallelTransport loop
    in sumElements (takeDiag mat) / fromIntegral (rows mat)

curvature2Form :: Loop a -> Loop a -> Matrix (Complex Double)
curvature2Form loop1 loop2 = 
    let h1 = holMatrix $ parallelTransport loop1
        h2 = holMatrix $ parallelTransport loop2
        commutator = h1 <> h2 - h2 <> h1
    in commutator

berryPhase :: [InfoState] -> Complex Double
berryPhase states = 
    sum $ zipWith innerProduct states (tail states ++ [head states])
  where
    innerProduct (InfoState v1) (InfoState v2) = 
        V.sum $ V.zipWith (*) (V.map conjugate v1) v2

nonAbelianHolonomy :: Loop a -> Loop a -> HolonomyOperator
nonAbelianHolonomy loop1 loop2 = HolonomyOperator
    { holMatrix = h1 <> h2 - h2 <> h1
    , holPhase = phase1 * phase2
    , holNonAbelian = True
    }
  where
    HolonomyOperator h1 phase1 _ = parallelTransport loop1
    HolonomyOperator h2 phase2 _ = parallelTransport loop2

holonomyGroup :: [Loop a] -> [HolonomyOperator]
holonomyGroup loops = map parallelTransport loops

data Connection a = Connection
    { connectionForm :: a -> Matrix (Complex Double)
    , connectionCurvature :: a -> a -> Matrix (Complex Double)
    }

flatConnection :: Connection a
flatConnection = Connection
    { connectionForm = \_ -> ident 2
    , connectionCurvature = \_ _ -> (2><2) [0,0,0,0]
    }

yangMillsConnection :: Double -> Connection a
yangMillsConnection coupling = Connection
    { connectionForm = \_ -> (2><2) [0, coupling :+ 0, coupling :+ 0, 0]
    , connectionCurvature = \_ _ -> 
        let g = coupling :+ 0
        in (2><2) [0, g*g, -(g*g), 0]
    }

holonomyAnomaly :: Loop a -> Loop a -> Complex Double
holonomyAnomaly loop1 loop2 = 
    let h12 = parallelTransport (fromJust $ composeLoops loop1 loop2)
        h1 = parallelTransport loop1
        h2 = parallelTransport loop2
        expected = holMatrix h1 <> holMatrix h2
        actual = holMatrix h12
        diff = actual - expected
    in sumElements (takeDiag (diff <> ctrans diff)) / fromIntegral (rows diff)
  where
    fromJust (Just x) = x
    fromJust Nothing = error "Loops cannot be composed"