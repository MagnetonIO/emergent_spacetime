{-# LANGUAGE RecordWildCards #-}

module InformationalSpacetime.Galaxy where

import InformationalSpacetime.Category
import InformationalSpacetime.Holonomy
import InformationalSpacetime.Geometry
import Data.Complex
import Numeric.LinearAlgebra hiding (identity)
import qualified Data.Map.Strict as Map

data GalaxyModel = GalaxyModel
    { galName :: String
    , galRadius :: Double
    , galMassDistribution :: Double -> Double
    , galInfoCategory :: InfoCategory
    , galHolonomy :: NonCommutativeHolonomy
    }

data RotationCurve = RotationCurve
    { rcRadius :: [Double]
    , rcVelocity :: [Double]
    , rcTheoretical :: [Double]
    }

newtonianVelocity :: Double -> Double -> Double -> Double
newtonianVelocity mass radius g = sqrt (g * mass / radius)

constructGalaxyCategory :: String -> Double -> InfoCategory
constructGalaxyCategory name maxRadius = InfoCategory
    { catObjects = galaxyObjects
    , catMorphisms = galaxyMorphisms
    , catComposition = compose
    , catIdentity = identity
    }
  where
    numRings = 20
    radii = [maxRadius * fromIntegral i / fromIntegral numRings | i <- [1..numRings]]
    
    galaxyObjects = [InfoObject 
        { objId = name ++ "_ring_" ++ show i
        , objData = Map.fromList [("radius", r), ("angle", 0)]
        } | (i, r) <- zip [1..numRings] radii]
    
    galaxyMorphisms = concatMap ringMorphisms (zip [0..] galaxyObjects)
    
    ringMorphisms (i, obj) = 
        let neighbors = [(i-1, "inward"), (i+1, "outward")]
            validNeighbors = filter (\(j,_) -> j >= 0 && j < length galaxyObjects) neighbors
        in [InfoMorphism
            { morphId = objId obj ++ "_to_" ++ objId (galaxyObjects !! j) ++ "_" ++ dir
            , source = obj
            , target = galaxyObjects !! j
            , transformation = \m -> Map.adjust (+ if dir == "inward" then -0.1 else 0.1) "radius" m
            } | (j, dir) <- validNeighbors] ++
           [identity obj]

computeHolonomicRotation :: GalaxyModel -> Double -> Double
computeHolonomicRotation GalaxyModel{..} radius = 
    let baseVel = newtonianVelocity (galMassDistribution radius) radius 6.67e-11
        holonomyCorrection = computeHolonomyContribution radius
    in baseVel * (1 + holonomyCorrection)
  where
    computeHolonomyContribution r = 
        let ringObj = findRingObject r
            loop = findRadialLoop ringObj
            holElem = (nchRepresentation galHolonomy) loop
            phase = imagPart $ log (holPhase holElem)
        in phase * r / galRadius
    
    findRingObject r = InfoObject
        { objId = galName ++ "_ring_approx"
        , objData = Map.fromList [("radius", r), ("angle", 0)]
        }
    
    findRadialLoop obj = []

generateRotationCurve :: GalaxyModel -> RotationCurve
generateRotationCurve model = RotationCurve
    { rcRadius = radii
    , rcVelocity = map (computeHolonomicRotation model) radii
    , rcTheoretical = map computeNewtonian radii
    }
  where
    radii = [galRadius model * fromIntegral i / 100 | i <- [5..100]]
    computeNewtonian r = newtonianVelocity (galMassDistribution model r) r 6.67e-11

data GalacticObstruction = GalacticObstruction
    { goInnerRegion :: [InfoObject]
    , goOuterRegion :: [InfoObject]
    , goMissingConnections :: [(InfoObject, InfoObject)]
    , goHolonomyPhase :: Double
    }

createGalacticObstruction :: GalaxyModel -> GalacticObstruction
createGalacticObstruction GalaxyModel{..} = GalacticObstruction
    { goInnerRegion = take 5 (catObjects galInfoCategory)
    , goOuterRegion = drop 15 (catObjects galInfoCategory)
    , goMissingConnections = findMissingConnections
    , goHolonomyPhase = 0.1
    }
  where
    findMissingConnections = 
        let innerObjs = take 5 (catObjects galInfoCategory)
            outerObjs = drop 15 (catObjects galInfoCategory)
        in [(i, o) | i <- innerObjs, o <- outerObjs, not (hasDirectConnection i o)]
    
    hasDirectConnection obj1 obj2 = any (\m -> 
        objId (source m) == objId obj1 && objId (target m) == objId obj2)
        (catMorphisms galInfoCategory)

flatRotationCurve :: Double -> Double -> Double -> Double
flatRotationCurve v0 r0 r = v0 * sqrt(1 + (r/r0)^(2::Int)) / (1 + r/r0)

tullyFisherRelation :: Double -> Double -> Double
tullyFisherRelation luminosity alpha = 200 * (luminosity / 1e10) ** (alpha / 4)

darkMatterProfile :: Double -> Double -> Double -> Double
darkMatterProfile rho0 rs r = rho0 / ((r/rs) * (1 + r/rs)^(2::Int))

informationalDensityProfile :: GalaxyModel -> Double -> Double
informationalDensityProfile GalaxyModel{..} radius = 
    let baseObj = InfoObject
            { objId = galName ++ "_probe"
            , objData = Map.fromList [("radius", radius), ("angle", 0)]
            }
        localPaths = findPaths baseObj (catObjects galInfoCategory) galInfoCategory
        avgHolonomy = if null localPaths 
                      then 0 
                      else sum (map pathComplexity localPaths) / fromIntegral (length localPaths)
    in avgHolonomy * exp(-radius / galRadius)
  where
    pathComplexity path = fromIntegral (length path) * 
                          sum [1 / fromIntegral i | i <- [1..length path]]

simulateGalaxyDynamics :: GalaxyModel -> Double -> [(Double, Double, Double)]
simulateGalaxyDynamics model time = 
    let numSteps = 1000
        dt = time / fromIntegral numSteps
        radii = [galRadius model * fromIntegral i / 20 | i <- [1..20]]
    in [(r, computeHolonomicRotation model r, angle) | 
        r <- radii,
        let omega = computeHolonomicRotation model r / r,
        let angle = omega * time]

compareWithObservations :: RotationCurve -> RotationCurve -> Double
compareWithObservations observed model = 
    let pairs = zip (rcVelocity observed) (rcVelocity model)
        squaredDiffs = [(vo - vm)^(2::Int) | (vo, vm) <- pairs]
    in sqrt (sum squaredDiffs / fromIntegral (length squaredDiffs))