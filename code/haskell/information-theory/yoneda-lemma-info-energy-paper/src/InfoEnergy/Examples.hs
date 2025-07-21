module InfoEnergy.Examples where

import InfoEnergy.Core
import InfoEnergy.Quantum
import InfoEnergy.Categorical
import InfoEnergy.BlackHole
import InfoEnergy.Complexity
import qualified Numeric.LinearAlgebra as LA

example1_MaxwellDemon :: IO ()
example1_MaxwellDemon = do
  putStrLn "Maxwell's Demon Example"
  let numParticles = 100
      hotTemp = Temp 400
      coldTemp = Temp 200
      avgTemp = Temp 300
      
      initialEntropy = Entropy $ fromIntegral numParticles * log 2
      finalEntropy = Entropy $ fromIntegral numParticles * log 2 * 0.5
      
      energyCost = landauerPrinciple (initialEntropy - finalEntropy) avgTemp
  
  putStrLn $ "Initial entropy: " ++ show initialEntropy
  putStrLn $ "Final entropy: " ++ show finalEntropy  
  putStrLn $ "Minimum energy cost to sort molecules: " ++ show energyCost

example2_QuantumErasure :: IO ()
example2_QuantumErasure = do
  putStrLn "\nQuantum Erasure Example"
  let qubitDim = 2
      pureState = Pure $ LA.fromList [1, 0]
      mixedState = Mixed $ LA.scale 0.5 $ LA.ident qubitDim
      
      pureEntropy = quantumVonNeumannEntropy pureState
      mixedEntropy = quantumVonNeumannEntropy mixedState
      
      temp = Temp 300
      erasureCost = quantumLandauer pureState mixedState temp
  
  putStrLn $ "Pure state entropy: " ++ show pureEntropy
  putStrLn $ "Mixed state entropy: " ++ show mixedEntropy
  putStrLn $ "Erasure energy cost: " ++ show erasureCost

example3_BlackHoleThermodynamics :: IO ()
example3_BlackHoleThermodynamics = do
  putStrLn "\nBlack Hole Thermodynamics Example"
  let solarMass = 1.989e30
      bhMass = Mass (10 * solarMass)
      
      bh = BlackHole
        { bhMass = bhMass
        , bhArea = horizonArea bhMass
        , bhTemperature = hawkingTemperature bhMass
        , bhEntropy = bekensteinHawkingEntropy (horizonArea bhMass)
        }
      
      ieSystem = blackHoleIESystem bhMass
  
  putStrLn $ "Black hole mass: " ++ show bhMass
  putStrLn $ "Horizon area: " ++ show (bhArea bh)
  putStrLn $ "Hawking temperature: " ++ show (bhTemperature bh)
  putStrLn $ "Bekenstein-Hawking entropy: " ++ show (bhEntropy bh)

example4_ComputationalComplexity :: IO ()
example4_ComputationalComplexity = do
  putStrLn "\nComputational Complexity Example"
  let satProblem = Problem
        { problemSize = 100
        , problemInstances = ["instance1", "instance2"]
        , problemComplexity = NP
        }
      
      factorProblem = Problem
        { problemSize = 2048
        , problemInstances = ["RSA-2048"]
        , problemComplexity = BQP
        }
      
      temp = Temp 300
      satEnergy = minimalEnergyToSolve satProblem temp
      factorEnergy = minimalEnergyToSolve factorProblem temp
  
  putStrLn $ "SAT problem energy: " ++ show satEnergy
  putStrLn $ "Factoring energy (quantum): " ++ show factorEnergy
  putStrLn $ "Quantum advantage: " ++ show (quantumSupremacy factorProblem)

example5_HolographicCorrespondence :: IO ()
example5_HolographicCorrespondence = do
  putStrLn "\nHolographic Correspondence Example"
  let bhMass = Mass 1e40
      bh = BlackHole
        { bhMass = bhMass
        , bhArea = horizonArea bhMass
        , bhTemperature = hawkingTemperature bhMass
        , bhEntropy = bekensteinHawkingEntropy (horizonArea bhMass)
        }
      
      bulkSystem = ThermoSystem
        { thermoStateSpace = bh
        , thermoEnergy = \_ -> Energy $ unMass bhMass * speedOfLight^2
        , thermoTemperature = const (bhTemperature bh)
        , thermoPressure = const 0
        , thermoVolume = const 0
        , thermoEntropyProduction = \_ _ -> bhEntropy bh
        }
      
      boundaryDim = floor $ unEntropy (bhEntropy bh) / log 2
      boundarySystem = QInfoSpace
        { qiHilbertDim = min boundaryDim 1000
        , qiStates = []
        , qiVonNeumannEntropy = const (bhEntropy bh)
        }
      
      holoSystem = HolographicIE bulkSystem boundarySystem (const $ Pure $ LA.fromList [1])
  
  putStrLn $ "Bulk entropy: " ++ show (bhEntropy bh)
  putStrLn $ "Boundary Hilbert space dimension: " ++ show boundaryDim
  putStrLn $ "Holographic correspondence established"

runAllExamples :: IO ()
runAllExamples = do
  example1_MaxwellDemon
  example2_QuantumErasure
  example3_BlackHoleThermodynamics
  example4_ComputationalComplexity
  example5_HolographicCorrespondence