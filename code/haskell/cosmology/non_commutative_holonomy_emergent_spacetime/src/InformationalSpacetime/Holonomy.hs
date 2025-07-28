{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module InformationalSpacetime.Holonomy where

import InformationalSpacetime.Category
import InformationalSpacetime.Geometry
import Data.Complex
import Numeric.LinearAlgebra hiding (identity)
import qualified Data.Map.Strict as Map
import Control.Monad.State

data HolonomyElement = HolonomyElement
    { holMatrix :: Matrix (Complex Double)
    , holPhase :: Complex Double
    , holPath :: Path
    } deriving (Show)

type HolonomyGroup = [HolonomyElement]

data NonCommutativeHolonomy = NonCommutativeHolonomy
    { nchBasePoint :: InfoObject
    , nchLoops :: [Path]
    , nchRepresentation :: Path -> HolonomyElement
    }

computeHolonomy :: Path -> InfoCategory -> HolonomyElement
computeHolonomy path cat = HolonomyElement
    { holMatrix = pathToMatrix path
    , holPhase = computePhase path
    , holPath = path
    }
  where
    pathToMatrix :: Path -> Matrix (Complex Double)
    pathToMatrix [] = ident 2
    pathToMatrix (m:ms) = morphToMatrix m <> pathToMatrix ms
    
    morphToMatrix :: InfoMorphism -> Matrix (Complex Double)
    morphToMatrix m = (2><2) 
        [ 1 :+ 0, phase :+ 0
        , 0 :+ 0, 1 :+ 0 ]
      where
        phase = fromIntegral (length (morphId m)) / 10
    
    computePhase :: Path -> Complex Double
    computePhase p = exp (0 :+ totalAngle)
      where
        totalAngle = sum $ map morphAngle p
        morphAngle m = fromIntegral (length (morphId m)) / 10

holonomyComposition :: HolonomyElement -> HolonomyElement -> HolonomyElement
holonomyComposition h1 h2 = HolonomyElement
    { holMatrix = holMatrix h1 <> holMatrix h2
    , holPhase = holPhase h1 * holPhase h2
    , holPath = holPath h1 ++ holPath h2
    }

holonomyInverse :: HolonomyElement -> HolonomyElement
holonomyInverse h = HolonomyElement
    { holMatrix = inv (holMatrix h)
    , holPhase = 1 / holPhase h
    , holPath = reverse (holPath h)
    }

isNonCommutative :: HolonomyGroup -> Bool
isNonCommutative group = or [not (commute h1 h2) | h1 <- group, h2 <- group]
  where
    commute h1 h2 = 
        let comp12 = holonomyComposition h1 h2
            comp21 = holonomyComposition h2 h1
        in norm_2 (flatten (holMatrix comp12 - holMatrix comp21)) < 1e-10

data HolonomyObstruction = HolonomyObstruction
    { obsObject :: InfoObject
    , obsPaths :: [Path]
    , obsHolonomies :: [HolonomyElement]
    , obsMissingConnections :: [(InfoObject, InfoObject)]
    }

computeObstruction :: InfoObject -> [InfoObject] -> ObstructedCategory -> HolonomyObstruction
computeObstruction baseObj targetObjs obsCat = HolonomyObstruction
    { obsObject = baseObj
    , obsPaths = findPaths baseObj targetObjs (obsCategory obsCat)
    , obsHolonomies = map (\p -> computeHolonomy p (obsCategory obsCat)) paths
    , obsMissingConnections = obsMissingHoms obsCat
    }
  where
    paths = findPaths baseObj targetObjs (obsCategory obsCat)

findPaths :: InfoObject -> [InfoObject] -> InfoCategory -> [Path]
findPaths start targets cat = concatMap (findPathsTo start) targets
  where
    findPathsTo :: InfoObject -> InfoObject -> [Path]
    findPathsTo from to = dfs from to [] []
    
    dfs :: InfoObject -> InfoObject -> [InfoObject] -> Path -> [Path]
    dfs current target visited currentPath
        | objId current == objId target = [reverse currentPath]
        | objId current `elem` map objId visited = []
        | otherwise = concatMap exploreMorph outgoing
      where
        outgoing = [m | m <- catMorphisms cat, objId (source m) == objId current]
        exploreMorph m = dfs (target m) target (current:visited) (m:currentPath)

wilsonLoop :: InfoObject -> Path -> InfoCategory -> Complex Double
wilsonLoop base loop cat = trace (holMatrix hol) / fromIntegral (rows (holMatrix hol))
  where
    hol = computeHolonomy loop cat
    
    trace :: Matrix (Complex Double) -> Complex Double
    trace m = sum [m `atIndex` (i,i) | i <- [0..min (rows m) (cols m) - 1]]

berryPhase :: Path -> InfoCategory -> Double
berryPhase path cat = imagPart $ log (holPhase hol)
  where
    hol = computeHolonomy path cat

nonCommutativeLemma :: InfoObject -> InfoObject -> InfoObject -> ObstructedCategory -> Bool
nonCommutativeLemma a b c obsCat = 
    hasAB && hasBA && not hasAC && pathDependent
  where
    cat = obsCategory obsCat
    morphs = catMorphisms cat
    
    hasAB = any (\m -> objId (source m) == objId a && objId (target m) == objId b) morphs
    hasBA = any (\m -> objId (source m) == objId b && objId (target m) == objId a) morphs
    hasAC = any (\m -> objId (source m) == objId a && objId (target m) == objId c) morphs
    
    pathDependent = case (findLoopABA, findAlternativePath) of
        (Just loopABA, Just altPath) ->
            let holABA = computeHolonomy loopABA cat
                holAlt = computeHolonomy altPath cat
            in norm_2 (flatten (holMatrix holABA - holMatrix holAlt)) > 1e-10
        _ -> False
    
    findLoopABA = do
        ab <- findMorph a b
        ba <- findMorph b a
        pathComposition [ab, ba]
    
    findAlternativePath = Nothing
    
    findMorph from to = listToMaybe [m | m <- morphs, 
                                         objId (source m) == objId from, 
                                         objId (target m) == objId to]
    
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

data HolonomyAlgebra = HolonomyAlgebra
    { algGenerators :: [Matrix (Complex Double)]
    , algProduct :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
    , algCommutator :: Matrix (Complex Double) -> Matrix (Complex Double) -> Matrix (Complex Double)
    }

makeHolonomyAlgebra :: HolonomyGroup -> HolonomyAlgebra
makeHolonomyAlgebra group = HolonomyAlgebra
    { algGenerators = map holMatrix group
    , algProduct = (<>)
    , algCommutator = \a b -> a <> b - b <> a
    }