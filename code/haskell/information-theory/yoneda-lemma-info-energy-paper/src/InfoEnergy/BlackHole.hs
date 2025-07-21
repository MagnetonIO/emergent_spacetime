module InfoEnergy.BlackHole where

import InfoEnergy.Core
import InfoEnergy.Quantum
import InfoEnergy.Categorical

newtype Area = Area Double deriving (Eq, Show)
newtype Mass = Mass Double deriving (Eq, Show)

planckLength :: Double
planckLength = 1.616255e-35

newtonG :: Double
newtonG = 6.67430e-11

speedOfLight :: Double
speedOfLight = 299792458

data BlackHole = BlackHole
  { bhMass :: Mass
  , bhArea :: Area
  , bhTemperature :: Temperature
  , bhEntropy :: Entropy
  }

data HolographicIESystem = HolographicIE
  { holoBulk :: ThermodynamicSystem BlackHole
  , holoBoundary :: QuantumInfoSpace
  , holoCorrespondence :: BlackHole -> QuantumState
  }

schwarzschildRadius :: Mass -> Double
schwarzschildRadius (Mass m) = 2 * newtonG * m / (speedOfLight * speedOfLight)

horizonArea :: Mass -> Area
horizonArea m = Area $ 4 * pi * r * r
  where r = schwarzschildRadius m

bekensteinHawkingEntropy :: Area -> Entropy
bekensteinHawkingEntropy (Area a) = 
  Entropy $ a / (4 * planckLength * planckLength)

hawkingTemperature :: Mass -> Temperature
hawkingTemperature (Mass m) = 
  let hbar = 1.054571817e-34
      k_b = boltzmannConstant
  in Temp $ hbar * speedOfLight^3 / (8 * pi * newtonG * m * k_b)

blackHoleIESystem :: Mass -> IESystem BlackHole BlackHole
blackHoleIESystem mass = IESystem info thermo id
  where
    bh = BlackHole 
      { bhMass = mass
      , bhArea = horizonArea mass
      , bhTemperature = hawkingTemperature mass
      , bhEntropy = bekensteinHawkingEntropy (horizonArea mass)
      }
    info = InfoSpace
      { infoStates = [bh]
      , infoProbDist = const (Prob 1.0)
      , infoEntropy = bhEntropy bh
      }
    thermo = ThermoSystem
      { thermoStateSpace = bh
      , thermoEnergy = \_ -> Energy $ unMass mass * speedOfLight * speedOfLight
      , thermoTemperature = const (bhTemperature bh)
      , thermoPressure = const 0
      , thermoVolume = \_ -> 4/3 * pi * (schwarzschildRadius mass)^3
      , thermoEntropyProduction = \_ _ -> bhEntropy bh
      }

holographicDuality :: HolographicIESystem -> IESystem QuantumState BlackHole
holographicDuality (HolographicIE bulk boundary corr) = 
  IESystem boundaryInfo bulk corr
  where
    boundaryInfo = InfoSpace
      { infoStates = qiStates boundary
      , infoProbDist = \state -> 
          let s = qiVonNeumannEntropy boundary state
          in Prob $ exp (- unEntropy s)
      , infoEntropy = Entropy 0
      }

informationParadoxResolution :: BlackHole -> (InformationSpace BlackHole, InformationSpace QuantumState)
informationParadoxResolution bh = (interior, exterior)
  where
    interior = InfoSpace
      { infoStates = [bh]
      , infoProbDist = const (Prob 1.0)
      , infoEntropy = bhEntropy bh
      }
    exterior = InfoSpace
      { infoStates = []
      , infoProbDist = const (Prob 0.0)
      , infoEntropy = bhEntropy bh
      }

pageTime :: Mass -> Double
pageTime (Mass m) = m^3 * newtonG^2 / (hbar * speedOfLight^4)
  where hbar = 1.054571817e-34

scrambllingTime :: Mass -> Double
scrambllingTime m = unTemp (hawkingTemperature m) * log (unMass m)

firewallProblem :: BlackHole -> Bool
firewallProblem bh = 
  let s = unEntropy (bhEntropy bh)
      maxEntanglement = s / 2
  in s > maxEntanglement