module Main where

import InformationalSpacetime.Category
import InformationalSpacetime.Holonomy
import InformationalSpacetime.Geometry
import InformationalSpacetime.Galaxy
import qualified Data.Map.Strict as Map
import Text.Printf

main :: IO ()
main = do
    putStrLn "Non-Commutative Holonomy in Emergent Spacetime - Galaxy Simulation"
    putStrLn "=================================================================="
    
    let galaxyName = "Milky Way Model"
    let maxRadius = 50000  -- 50 kpc in parsecs
    
    putStrLn $ "\nConstructing informational category for " ++ galaxyName
    let galCat = constructGalaxyCategory galaxyName maxRadius
    
    putStrLn $ "Created " ++ show (length (catObjects galCat)) ++ " informational objects"
    putStrLn $ "Created " ++ show (length (catMorphisms galCat)) ++ " morphisms"
    
    putStrLn "\nChecking category transitivity..."
    let isTransitive = checkTransitivity galCat
    putStrLn $ "Category is transitive: " ++ show isTransitive
    
    putStrLn "\nConstructing galaxy model with non-commutative holonomy..."
    let galaxy = GalaxyModel
            { galName = galaxyName
            , galRadius = maxRadius
            , galMassDistribution = \r -> 1e12 * exp(-r/10000)  -- Exponential disk
            , galInfoCategory = galCat
            , galHolonomy = NonCommutativeHolonomy
                { nchBasePoint = head (catObjects galCat)
                , nchLoops = []
                , nchRepresentation = computeHolonomy [] galCat
                }
            }
    
    putStrLn "\nGenerating rotation curve..."
    let rotCurve = generateRotationCurve galaxy
    
    putStrLn "\nRadius (kpc) | Observed V (km/s) | Newtonian V (km/s) | Ratio"
    putStrLn "--------------------------------------------------------------"
    
    mapM_ printRotationPoint $ zip3 (rcRadius rotCurve) 
                                    (rcVelocity rotCurve) 
                                    (rcTheoretical rotCurve)
    
    putStrLn "\nCreating galactic obstruction model..."
    let obstruction = createGalacticObstruction galaxy
    putStrLn $ "Missing connections: " ++ show (length (goMissingConnections obstruction))
    putStrLn $ "Holonomy phase: " ++ printf "%.4f" (goHolonomyPhase obstruction)
    
    putStrLn "\nDemonstrating Non-Commutative Holonomy Lemma:"
    let objs = catObjects galCat
    if length objs >= 3
        then do
            let a = objs !! 0
            let b = objs !! 1  
            let c = objs !! 10
            
            let obsCat = ObstructedCategory galCat [(a, c)]
            let lemmaHolds = nonCommutativeLemma a b c obsCat
            
            putStrLn $ "Objects: A = " ++ objId a ++ ", B = " ++ objId b ++ ", C = " ++ objId c
            putStrLn $ "Non-commutative holonomy lemma holds: " ++ show lemmaHolds
        else putStrLn "Not enough objects to demonstrate lemma"
    
    putStrLn "\nComputing informational density profile..."
    let radialPoints = [5000, 10000, 20000, 30000, 40000]
    putStrLn "Radius (kpc) | Information Density"
    putStrLn "-----------------------------------"
    mapM_ (\r -> printf "%12.0f | %.6f\n" r (informationalDensityProfile galaxy r)) radialPoints
    
    putStrLn "\nSimulation complete."

printRotationPoint :: (Double, Double, Double) -> IO ()
printRotationPoint (r, vObs, vNewt) = 
    printf "%12.1f | %17.1f | %18.1f | %5.2f\n" 
           (r/1000) vObs vNewt (vObs/vNewt)