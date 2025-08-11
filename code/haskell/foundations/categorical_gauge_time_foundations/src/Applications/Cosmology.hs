{-# LANGUAGE RecordWildCards #-}

module Applications.Cosmology where

import Numeric.LinearAlgebra
import Data.Complex
import Physics.WheelerDeWitt

data CosmologicalModel = CosmologicalModel
    { cmScaleFactor :: Double -> Double
    , cmHubbleParameter :: Double -> Double
    , cmEnergyDensity :: Double -> Double
    , cmEquationOfState :: Double -> Double
    }

friedmannEquations :: Double -> Double -> Double -> CosmologicalModel
friedmannEquations k rho_0 lambda = CosmologicalModel
    { cmScaleFactor = \t -> exp (sqrt (lambda / 3) * t)
    , cmHubbleParameter = \t -> sqrt (lambda / 3 + k / (exp (2 * sqrt (lambda / 3) * t)))
    , cmEnergyDensity = \t -> rho_0 / (exp (3 * sqrt (lambda / 3) * t))
    , cmEquationOfState = const (-1)
    }

data InflationaryModel = InflationaryModel
    { imInflaton :: Double -> Double
    , imPotential :: Double -> Double
    , imSlowRollParameters :: (Double, Double)
    , imEFolds :: Double
    }

slowRollInflation :: (Double -> Double) -> InflationaryModel
slowRollInflation potential = InflationaryModel
    { imInflaton = \t -> sqrt (2 * epsilon) * t
    , imPotential = potential
    , imSlowRollParameters = (epsilon, eta)
    , imEFolds = 60
    }
  where
    epsilon = 0.01
    eta = 0.01

data PrimordialFluctuations = PrimordialFluctuations
    { pfScalarPowerSpectrum :: Double -> Double
    , pfTensorPowerSpectrum :: Double -> Double
    , pfSpectralIndex :: Double
    , pfTensorToScalarRatio :: Double
    }

computeFluctuations :: InflationaryModel -> PrimordialFluctuations
computeFluctuations InflationaryModel{..} = PrimordialFluctuations
    { pfScalarPowerSpectrum = \k -> amplitude * (k / k_pivot) ** (n_s - 1)
    , pfTensorPowerSpectrum = \k -> r * amplitude * (k / k_pivot) ** n_t
    , pfSpectralIndex = n_s
    , pfTensorToScalarRatio = r
    }
  where
    amplitude = 2.2e-9
    k_pivot = 0.05
    (epsilon, eta) = imSlowRollParameters
    n_s = 1 - 6 * epsilon + 2 * eta
    n_t = -2 * epsilon
    r = 16 * epsilon

data DarkEnergy = DarkEnergy
    { deEquationOfState :: Double -> Double
    , deEnergyDensity :: Double -> Double
    , dePressure :: Double -> Double
    }

quintessenceModel :: Double -> Double -> DarkEnergy
quintessenceModel w0 wa = DarkEnergy
    { deEquationOfState = \a -> w0 + wa * (1 - a)
    , deEnergyDensity = \a -> rho_de0 * a ** (-3 * (1 + w0 + wa)) * exp (3 * wa * (a - 1))
    , dePressure = \a -> deEquationOfState (w0 + wa * (1 - a)) a * deEnergyDensity rho_de0 a
    }
  where
    rho_de0 = 0.7

data QuantumCosmologyState = QuantumCosmologyState
    { qcsWaveFunction :: Double -> Complex Double
    , qcsBoundaryCondition :: BoundaryCondition
    , qcsClassicalLimit :: Double -> Bool
    }

noboundaryProposal :: Double -> QuantumCosmologyState
noboundaryProposal lambda = QuantumCosmologyState
    { qcsWaveFunction = hartleHawkingWF lambda
    , qcsBoundaryCondition = HartleHawking
    , qcsClassicalLimit = \a -> a > a_classical
    }
  where
    hartleHawkingWF l a = exp (-(l * a^4 / 12) :+ 0)
    a_classical = 1.0

tunnelingProposal :: Double -> QuantumCosmologyState
tunnelingProposal lambda = QuantumCosmologyState
    { qcsWaveFunction = vilenkinWF lambda
    , qcsBoundaryCondition = Tunneling
    , qcsClassicalLimit = \a -> a > a_classical
    }
  where
    vilenkinWF l a = exp (0 :+ (l * a^4 / 12))
    a_classical = 1.0

data EternalInflation = EternalInflation
    { eiPocketUniverses :: Int
    , eiNucleationRate :: Double
    , eiFractalDimension :: Double
    , eiMeasureProblem :: String
    }

multiverseLandscape :: Int -> Double -> EternalInflation
multiverseLandscape pockets rate = EternalInflation
    { eiPocketUniverses = pockets
    , eiNucleationRate = rate
    , eiFractalDimension = 2.5
    , eiMeasureProblem = "Scale factor cutoff"
    }

data ArrowOfTime = ArrowOfTime
    { atThermodynamic :: Double -> Double
    , atCosmological :: Double -> Double
    , atQuantum :: Double -> Double
    , atEmergence :: Double
    }

entropyArrow :: Double -> ArrowOfTime
entropyArrow t_emergence = ArrowOfTime
    { atThermodynamic = \t -> if t > t_emergence then t else 0
    , atCosmological = \t -> t
    , atQuantum = \t -> 1 - exp (-t / t_emergence)
    , atEmergence = t_emergence
    }

data BigBangSingularity = BigBangSingularity
    { bbsRegularization :: Double -> Double
    , bbsQuantumBounce :: Bool
    , bbsPreBigBang :: Maybe CosmologicalModel
    }

quantumBounce :: Double -> BigBangSingularity
quantumBounce l_planck = BigBangSingularity
    { bbsRegularization = \a -> max a l_planck
    , bbsQuantumBounce = True
    , bbsPreBigBang = Just $ friedmannEquations (-1) 1.0 0.0
    }