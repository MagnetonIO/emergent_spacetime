{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Category.GaugeTime where

import Category.Base
import qualified Data.Set as Set
import Data.Complex
import Linear.V3
import Linear.Matrix

data Configuration = Configuration
    { configFields :: [V3 Double]
    , configMetric :: Matrix Double
    , configIndex :: Int
    } deriving (Eq, Show)

data GaugeTransformation = GaugeTransformation
    { gaugeParameter :: Double -> V3 Double
    , gaugeGroup :: String
    , gaugeMatrix :: Matrix (Complex Double)
    } deriving (Show)

instance Category GaugeTimeCategory where
    type Ob GaugeTimeCategory = Configuration
    type Mor GaugeTimeCategory = GaugeTransformation
    
    idMor = GaugeTransformation 
        { gaugeParameter = const (V3 0 0 0)
        , gaugeGroup = "Identity"
        , gaugeMatrix = identity 4
        }
    
    compose g2 g1 = GaugeTransformation
        { gaugeParameter = \t -> gaugeParameter g1 t + gaugeParameter g2 t
        , gaugeGroup = gaugeGroup g1 ++ "∘" ++ gaugeGroup g2
        , gaugeMatrix = gaugeMatrix g2 * gaugeMatrix g1
        }
    
    source _ = Configuration [] (identity 4) 0
    target _ = Configuration [] (identity 4) 1

data GaugeTimeCategory

instance Groupoid GaugeTimeCategory where
    inverse g = GaugeTransformation
        { gaugeParameter = \t -> negate (gaugeParameter g t)
        , gaugeGroup = "Inverse(" ++ gaugeGroup g ++ ")"
        , gaugeMatrix = inv (gaugeMatrix g)
        }
    
    inverseLaw1 _ = undefined
    inverseLaw2 _ = undefined

applyGaugeTransformation :: GaugeTransformation -> Configuration -> Configuration
applyGaugeTransformation GaugeTransformation{..} config =
    config { configFields = map transformField (configFields config) }
  where
    transformField field = field + gaugeParameter 0.0

data GaugeOrbit = GaugeOrbit
    { orbitRepresentative :: Configuration
    , orbitStabilizer :: [GaugeTransformation]
    , orbitDimension :: Int
    }

computeGaugeOrbit :: Configuration -> GaugeOrbit
computeGaugeOrbit config = GaugeOrbit
    { orbitRepresentative = config
    , orbitStabilizer = [idMor]
    , orbitDimension = 1
    }

data ModuliSpace = ModuliSpace
    { moduliOrbits :: Set.Set GaugeOrbit
    , moduliTopology :: String
    }

quotientByGauge :: Set.Set Configuration -> ModuliSpace
quotientByGauge configs = ModuliSpace
    { moduliOrbits = Set.map computeGaugeOrbit configs
    , moduliTopology = "Quotient"
    }

data Observable a = Observable
    { obsFunction :: Configuration -> a
    , obsIsGaugeInvariant :: Bool
    }

checkGaugeInvariance :: Eq a => Observable a -> GaugeTransformation -> Configuration -> Bool
checkGaugeInvariance obs g config =
    obsFunction obs config == obsFunction obs (applyGaugeTransformation g config)

data TemporalPath = TemporalPath
    { pathParameter :: Double -> Configuration
    , pathInterval :: (Double, Double)
    , pathGaugeClass :: [GaugeTransformation]
    }

equivalentPaths :: TemporalPath -> TemporalPath -> Bool
equivalentPaths path1 path2 =
    pathInterval path1 == pathInterval path2

emergentTime :: TemporalPath -> Double -> Configuration
emergentTime path t = pathParameter path t