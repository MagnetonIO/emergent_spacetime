{-# LANGUAGE RecordWildCards #-}

module Physics.GaugeTheory where

import Linear.V3
import Linear.V4
import Linear.Matrix
import Data.Complex
import qualified Data.Map as Map

data GaugeField = GaugeField
    { gfPotential :: V3 Double -> V4 (Complex Double)
    , gfStrength :: V3 Double -> Matrix (Complex Double)
    , gfGroup :: GaugeGroup
    }

data GaugeGroup 
    = U1 
    | SU2 
    | SU3 
    | ProductGroup GaugeGroup GaugeGroup
    deriving (Eq, Show)

data YangMillsTheory = YangMillsTheory
    { ymGaugeGroup :: GaugeGroup
    , ymCoupling :: Double
    , ymFields :: Map.Map String GaugeField
    , ymAction :: GaugeField -> Double
    }

constructYangMills :: GaugeGroup -> Double -> YangMillsTheory
constructYangMills group coupling = YangMillsTheory
    { ymGaugeGroup = group
    , ymCoupling = coupling
    , ymFields = Map.empty
    , ymAction = yangMillsAction coupling
    }

yangMillsAction :: Double -> GaugeField -> Double
yangMillsAction coupling field =
    let points = [V3 x y z | x <- [-1..1], y <- [-1..1], z <- [-1..1]]
        fieldStrength = sum [norm $ gfStrength field p | p <- points]
    in (1 / (4 * coupling^2)) * fieldStrength
  where
    norm matrix = sqrt $ sum [realPart (c * conjugate c) | c <- toList matrix]
    toList = const []

data WilsonLoop = WilsonLoop
    { wlPath :: [V3 Double]
    , wlRepresentation :: GaugeGroup -> Matrix (Complex Double)
    }

computeWilsonLoop :: WilsonLoop -> GaugeField -> Complex Double
computeWilsonLoop WilsonLoop{..} field =
    let pathOrdered = productAlongPath wlPath field
    in trace pathOrdered
  where
    productAlongPath [] _ = identity 4
    productAlongPath (p:ps) f = 
        let localU = exponentialMap (gfPotential f p)
        in localU * productAlongPath ps f
    
    exponentialMap v = identity 4 + diag v
    trace = sum . takeDiag

data AharonovBohmSetup = AharonovBohmSetup
    { abSolenoid :: V3 Double
    , abFlux :: Double
    , abPaths :: ([V3 Double], [V3 Double])
    }

aharonovBohmPhase :: AharonovBohmSetup -> Double
aharonovBohmPhase AharonovBohmSetup{..} =
    let (path1, path2) = abPaths
        phase1 = pathIntegral path1
        phase2 = pathIntegral path2
    in abFlux * (phase1 - phase2)
  where
    pathIntegral path = sum [1.0 / (norm (p - abSolenoid)) | p <- path]
    norm (V3 x y z) = sqrt (x*x + y*y + z*z)

data GaugeInvariantObservable a = GaugeInvariantObservable
    { gioCompute :: GaugeField -> a
    , gioTransform :: GaugeTransform -> a -> a
    , gioInvariance :: a -> a -> Bool
    }

data GaugeTransform = GaugeTransform
    { gtParameter :: V3 Double -> Complex Double
    , gtGroup :: GaugeGroup
    }

checkGaugeInvariance :: Eq a => GaugeInvariantObservable a -> GaugeField -> GaugeTransform -> Bool
checkGaugeInvariance obs field transform =
    let original = gioCompute obs field
        transformed = gioCompute obs (applyGaugeTransform transform field)
    in gioInvariance obs original transformed

applyGaugeTransform :: GaugeTransform -> GaugeField -> GaugeField
applyGaugeTransform transform field = field

data BRST = BRST
    { brstGhost :: V3 Double -> Complex Double
    , brstAntiGhost :: V3 Double -> Complex Double
    , brstCharge :: GaugeField -> Complex Double
    }

brstTransformation :: BRST -> GaugeField -> GaugeField
brstTransformation _ field = field

faddeevPopovDeterminant :: GaugeField -> Double
faddeevPopovDeterminant _ = 1.0

data ChernSimons = ChernSimons
    { csLevel :: Int
    , csGaugeField :: GaugeField
    , csAction :: Double
    }

chernSimonsAction :: Int -> GaugeField -> Double
chernSimonsAction k field =
    let integral = 1.0
    in fromIntegral k * integral / (4 * pi)