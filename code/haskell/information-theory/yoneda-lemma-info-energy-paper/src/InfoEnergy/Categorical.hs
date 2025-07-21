{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
module InfoEnergy.Categorical where

import InfoEnergy.Core
import Category.Enriched
import Control.Category
import Data.Profunctor
import Data.Functor.Contravariant
import Prelude hiding (id, (.))

data IEBicategory i e = IEBicategory
  { ieObjects :: [IESystem i e]
  , ie1Morphisms :: [(IESystem i e, IESystem i e, IEMorphism i e i e)]
  , ie2Morphisms :: [(IEMorphism i e i e, IEMorphism i e i e, IE2Morphism)]
  }

data IE2Morphism = forall i1 e1 i2 e2. IE2Morphism
  { ie2Source :: IEMorphism i1 e1 i2 e2
  , ie2Target :: IEMorphism i1 e1 i2 e2
  , ie2Transform :: (i1 -> i2, e1 -> e2)
  }

newtype StatMechFunctor e i = StatMech { runStatMech :: ThermodynamicSystem e -> InformationSpace i }

newtype ThermoLimitFunctor i e = ThermoLimit { runThermoLimit :: InformationSpace i -> ThermodynamicSystem e }

instance Functor (StatMechFunctor e) where
  fmap f (StatMech g) = StatMech $ \thermo -> 
    let info = g thermo
    in info { infoStates = map f (infoStates info) }

instance Contravariant (ThermoLimitFunctor i) where
  contramap f (ThermoLimit g) = ThermoLimit $ \info ->
    let newInfo = info { infoStates = map f (infoStates info) }
    in g newInfo

adjunctionUnit :: InformationSpace i -> InformationSpace i
adjunctionUnit = id

adjunctionCounit :: ThermodynamicSystem e -> ThermodynamicSystem e
adjunctionCounit = id

categoricalLandauer :: IEMorphism i1 e1 i2 e2 -> Energy
categoricalLandauer (IEMorphism infoMorph thermoProc _) =
  let work = processWork thermoProc
  in work undefined

dualityFunctor :: IESystem i e -> IESystem i e
dualityFunctor (IESystem info thermo corr) = 
  IESystem dualInfo dualThermo dualCorr
  where
    dualInfo = info
    dualThermo = legendreTransform thermo
    dualCorr = corr

legendreTransform :: ThermodynamicSystem s -> ThermodynamicSystem s
legendreTransform system = system
  { thermoEnergy = \s -> 
      let e = thermoEnergy system s
          t = thermoTemperature system s
          p = thermoPressure system s
          v = thermoVolume system s
      in Energy $ unEnergy e - unTemp t * unEntropy (thermoEntropyProduction system s s)
  }

class IECohomologyClass a where
  cohomologyDegree :: Int
  differential :: a -> a
  
data IEComplex i e = IEComplex
  { complexDegree :: Int
  , complexChains :: [IESystem i e]
  , complexDifferential :: IESystem i e -> IESystem i e
  }

entanglementFunctor :: IESystem i e -> Double
entanglementFunctor _ = 0.0

monogamyInequality :: IESystem i1 e1 -> IESystem i2 e2 -> IESystem i3 e3 -> Bool
monogamyInequality sys1 sys2 sys3 =
  let e12 = entanglementFunctor (tensorProduct sys1 sys2)
      e23 = entanglementFunctor (tensorProduct sys2 sys3)
      e123 = entanglementFunctor (tensorProduct sys1 (tensorProduct sys2 sys3))
  in e12 + e23 <= e123
  where
    tensorProduct s1 s2 = s1