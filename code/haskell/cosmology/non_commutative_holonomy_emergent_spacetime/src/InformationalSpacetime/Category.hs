{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module InformationalSpacetime.Category where

import Data.Kind (Type)
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

data InfoObject = InfoObject
    { objId :: String
    , objData :: Map String Double
    } deriving (Show, Eq)

data InfoMorphism where
    InfoMorphism :: 
        { morphId :: String
        , source :: InfoObject
        , target :: InfoObject
        , transformation :: Map String Double -> Map String Double
        } -> InfoMorphism

instance Show InfoMorphism where
    show m = "Morphism " ++ morphId m ++ ": " ++ objId (source m) ++ " -> " ++ objId (target m)

data InfoCategory = InfoCategory
    { catObjects :: [InfoObject]
    , catMorphisms :: [InfoMorphism]
    , catComposition :: InfoMorphism -> InfoMorphism -> Maybe InfoMorphism
    , catIdentity :: InfoObject -> InfoMorphism
    }

compose :: InfoMorphism -> InfoMorphism -> Maybe InfoMorphism
compose f g
    | objId (target g) == objId (source f) = Just $ InfoMorphism
        { morphId = morphId g ++ ";" ++ morphId f
        , source = source g
        , target = target f
        , transformation = transformation f . transformation g
        }
    | otherwise = Nothing

identity :: InfoObject -> InfoMorphism
identity obj = InfoMorphism
    { morphId = "id_" ++ objId obj
    , source = obj
    , target = obj
    , transformation = id
    }

data Functor f g = Functor
    { functorObjMap :: InfoObject -> InfoObject
    , functorMorphMap :: InfoMorphism -> InfoMorphism
    }

data NaturalTransformation f g = NaturalTransformation
    { natTransComponents :: InfoObject -> InfoMorphism
    }

type Path = [InfoMorphism]

pathComposition :: Path -> Maybe InfoMorphism
pathComposition [] = Nothing
pathComposition [m] = Just m
pathComposition (m:ms) = do
    rest <- pathComposition ms
    compose m rest

data HomSet a b = HomSet
    { homMorphisms :: [InfoMorphism]
    , homSource :: InfoObject
    , homTarget :: InfoObject
    }

isIsomorphism :: InfoMorphism -> InfoCategory -> Bool
isIsomorphism f cat = any isInverse (catMorphisms cat)
  where
    isInverse g = case (compose f g, compose g f) of
        (Just fg, Just gf) ->
            morphId fg == morphId (catIdentity cat (source f)) &&
            morphId gf == morphId (catIdentity cat (target f))
        _ -> False

data Groupoid = Groupoid
    { grpCategory :: InfoCategory
    , grpInverse :: InfoMorphism -> Maybe InfoMorphism
    }

makeGroupoid :: InfoCategory -> Groupoid
makeGroupoid cat = Groupoid
    { grpCategory = cat
    , grpInverse = \m -> findInverse m (catMorphisms cat)
    }
  where
    findInverse m ms = listToMaybe [g | g <- ms, isInversePair m g cat]
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x
    
    isInversePair f g c = case (compose f g, compose g f) of
        (Just fg, Just gf) ->
            morphId fg == morphId (catIdentity c (source f)) &&
            morphId gf == morphId (catIdentity c (target f))
        _ -> False

data ObstructedCategory = ObstructedCategory
    { obsCategory :: InfoCategory
    , obsMissingHoms :: [(InfoObject, InfoObject)]
    }

checkTransitivity :: InfoCategory -> Bool
checkTransitivity cat = all transitiveTriple triples
  where
    objs = catObjects cat
    triples = [(a,b,c) | a <- objs, b <- objs, c <- objs]
    
    transitiveTriple (a,b,c) =
        let hasAB = any (\m -> objId (source m) == objId a && objId (target m) == objId b) (catMorphisms cat)
            hasBC = any (\m -> objId (source m) == objId b && objId (target m) == objId c) (catMorphisms cat)
            hasAC = any (\m -> objId (source m) == objId a && objId (target m) == objId c) (catMorphisms cat)
        in not (hasAB && hasBC) || hasAC