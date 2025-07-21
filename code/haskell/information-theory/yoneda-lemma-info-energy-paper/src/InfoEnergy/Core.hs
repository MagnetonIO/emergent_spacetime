{-# LANGUAGE DerivingStrategies #-}
module InfoEnergy.Core where

import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import Control.Monad.State
import Data.Profunctor

newtype Probability = Prob { unProb :: Double }
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating)

newtype Entropy = Entropy { unEntropy :: Double }
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating)

newtype Energy = Energy { unEnergy :: Double }
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating)

newtype Temperature = Temp { unTemp :: Double }
  deriving newtype (Eq, Ord, Show, Num, Fractional, Floating)

boltzmannConstant :: Double
boltzmannConstant = 1.380649e-23

data InformationSpace a = InfoSpace
  { infoStates :: [a]
  , infoProbDist :: a -> Probability
  , infoEntropy :: Entropy
  }

data ThermodynamicSystem s = ThermoSystem
  { thermoStateSpace :: s
  , thermoEnergy :: s -> Energy
  , thermoTemperature :: s -> Temperature
  , thermoPressure :: s -> Double
  , thermoVolume :: s -> Double
  , thermoEntropyProduction :: s -> s -> Entropy
  }

data InformationMorphism a b = InfoMorphism
  { infoMap :: a -> b
  , infoPreservesProb :: Bool
  }

data ThermodynamicProcess s t = ThermoProcess
  { processMap :: s -> t
  , processWork :: s -> Energy
  , processHeatFlow :: s -> Energy
  }

data IESystem i e = IESystem
  { ieInfo :: InformationSpace i
  , ieThermo :: ThermodynamicSystem e
  , ieCorrespondence :: e -> i
  }

data IEMorphism i1 e1 i2 e2 = IEMorphism
  { ieMorphInfo :: InformationMorphism i1 i2
  , ieMorphThermo :: ThermodynamicProcess e1 e2
  , ieMorphCommutes :: Bool
  }

entropy :: (Foldable f) => f Probability -> Entropy
entropy probs = Entropy $ negate $ sum
  [ p * logBase 2 p | Prob p <- toList probs, p > 0 ]
  where toList = foldr (:) []

shannonEntropy :: InformationSpace a -> Entropy
shannonEntropy space = entropy 
  [ infoProbDist space s | s <- infoStates space ]

vonNeumannEntropy :: V.Vector (V.Vector Double) -> Entropy
vonNeumannEntropy densityMatrix = 
  let eigenvals = [] 
  in entropy [ Prob ev | ev <- eigenvals, ev > 0 ]

gibbsDistribution :: ThermodynamicSystem s -> [s] -> s -> Probability
gibbsDistribution system states s =
  let beta = 1 / (boltzmannConstant * unTemp (thermoTemperature system s))
      e = unEnergy (thermoEnergy system s)
      z = sum [ exp (-beta * unEnergy (thermoEnergy system s')) | s' <- states ]
  in Prob $ exp (-beta * e) / z

landauerPrinciple :: Entropy -> Temperature -> Energy
landauerPrinciple (Entropy deltaH) (Temp t) = 
  Energy $ boltzmannConstant * t * log 2 * deltaH

data IECohomology n a = IECohomology
  { cohomologyDegree :: Int
  , cohomologyGroup :: [a]
  , differential :: a -> a
  }

entropyClass :: IESystem i e -> IECohomology 1 Double
entropyClass (IESystem info thermo corr) = IECohomology
  { cohomologyDegree = 1
  , cohomologyGroup = [unEntropy $ infoEntropy info]
  , differential = id
  }