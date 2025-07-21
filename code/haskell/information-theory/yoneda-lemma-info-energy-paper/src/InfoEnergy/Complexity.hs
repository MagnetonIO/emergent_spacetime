module InfoEnergy.Complexity where

import InfoEnergy.Core
import InfoEnergy.Categorical
import Data.List (minimumBy)
import Data.Ord (comparing)

data ComplexityClass = P | NP | PSPACE | EXP | BQP | QMA
  deriving (Eq, Show, Ord)

data ComputationalProblem = Problem
  { problemSize :: Int
  , problemInstances :: [String]
  , problemComplexity :: ComplexityClass
  }

data ComplexityIESystem = ComplexityIE
  { compInfo :: InformationSpace ComputationalProblem
  , compThermo :: ThermodynamicSystem ComputationalResource
  , compCorrespondence :: ComputationalResource -> ComputationalProblem
  }

data ComputationalResource = CompResource
  { resourceTime :: Double
  , resourceSpace :: Double
  , resourceEnergy :: Energy
  }

kolmogorovComplexity :: String -> Int
kolmogorovComplexity s = length (compress s)
  where compress = id

algorithmicEntropy :: String -> Entropy
algorithmicEntropy s = Entropy $ fromIntegral (kolmogorovComplexity s) / fromIntegral (length s)

complexityEntropy :: ComplexityClass -> Entropy
complexityEntropy P = Entropy 1
complexityEntropy NP = Entropy 2  
complexityEntropy PSPACE = Entropy 3
complexityEntropy EXP = Entropy 4
complexityEntropy BQP = Entropy 2.5
complexityEntropy QMA = Entropy 3.5

minimalEnergyToSolve :: ComputationalProblem -> Temperature -> Energy
minimalEnergyToSolve prob temp = 
  let complexity = problemComplexity prob
      entropyChange = complexityEntropy complexity
  in landauerPrinciple entropyChange temp

computationalIESystem :: ComputationalProblem -> ComplexityIESystem
computationalIESystem prob = ComplexityIE info thermo corr
  where
    info = InfoSpace
      { infoStates = [prob]
      , infoProbDist = const (Prob 1.0)
      , infoEntropy = complexityEntropy (problemComplexity prob)
      }
    thermo = ThermoSystem
      { thermoStateSpace = CompResource 0 0 (Energy 0)
      , thermoEnergy = resourceEnergy
      , thermoTemperature = const (Temp 300)
      , thermoPressure = const 0
      , thermoVolume = resourceSpace
      , thermoEntropyProduction = \r1 r2 -> 
          Entropy $ unEnergy (resourceEnergy r2 - resourceEnergy r1) / 300
      }
    corr = const prob

quantumSupremacy :: ComputationalProblem -> Bool
quantumSupremacy prob = 
  case problemComplexity prob of
    BQP -> not (prob `solveableIn` P)
    _ -> False
  where
    solveableIn p c = True

oracleSeparation :: ComplexityClass -> ComplexityClass -> Bool
oracleSeparation c1 c2 = c1 /= c2

complexityHierarchy :: [ComplexityClass]
complexityHierarchy = [P, NP, PSPACE, EXP]

data Circuit = Circuit
  { circuitGates :: Int
  , circuitDepth :: Int
  , circuitQubits :: Int
  }

circuitComplexity :: Circuit -> Int
circuitComplexity c = circuitGates c * circuitDepth c

quantumCircuitEntropy :: Circuit -> Entropy  
quantumCircuitEntropy c = Entropy $ fromIntegral (circuitQubits c) * log 2

thermodynamicComplexity :: Energy -> Temperature -> Entropy -> Double
thermodynamicComplexity (Energy e) (Temp t) (Entropy s) = 
  e / (boltzmannConstant * t) - s

complexityPhaseTransition :: ComplexityClass -> ComplexityClass -> Temperature
complexityPhaseTransition c1 c2 = Temp $ 
  abs (unEntropy (complexityEntropy c2) - unEntropy (complexityEntropy c1)) * 100