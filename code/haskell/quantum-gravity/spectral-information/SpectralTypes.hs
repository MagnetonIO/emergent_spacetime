{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module SpectralTypes where

import Data.Complex
import GHC.Generics (Generic)
import Numeric.LinearAlgebra hiding (Complex)
import qualified Numeric.LinearAlgebra as LA

-- | Speed of light constant (normalized units)
speedOfLight :: Double
speedOfLight = 1.0

-- | Planck mass (normalized units)
planckMass :: Double  
planckMass = 1.0

-- | Information density type
newtype InformationDensity = InformationDensity Double
  deriving (Show, Eq, Ord, Num, Fractional, Floating)

-- | Energy type
newtype Energy = Energy Double
  deriving (Show, Eq, Ord, Num, Fractional, Floating)

-- | Eigenvalue type for information operator
data Eigenvalue = 
    RealEigenvalue Double
  | ComplexEigenvalue (Complex Double)
  deriving (Show, Eq, Generic)

-- | Spectrum classification
data SpectrumType = 
    DiscreteSpectrum    -- Bound states (localized information patterns)
  | ContinuousSpectrum  -- Scattering states (propagating disturbances)
  | ResonanceSpectrum   -- Metastable configurations
  deriving (Show, Eq, Generic)

-- | Information field configuration
data InformationField = InformationField
  { fieldDimension :: Int
  , localDensity   :: Vector Double           -- I(x) in the paper
  , diffusionTensor :: Matrix Double          -- D(x) in the paper
  } deriving (Show, Generic)

-- | Wave function for information patterns
data WaveFunction = WaveFunction
  { amplitude :: Vector (Complex Double)      -- ψ(x) in the paper
  , domain    :: Vector Double                -- spatial coordinates
  } deriving (Show, Generic)

-- | Information operator representation
data InformationOperator = InformationOperator
  { operatorMatrix :: Matrix (Complex Double)
  , boundaryCondition :: BoundaryCondition
  , threshold :: Double                       -- ε_threshold separating discrete/continuous
  } deriving (Show, Generic)

-- | Boundary conditions for the operator
data BoundaryCondition = 
    Dirichlet                                -- Zero at boundaries
  | Neumann                                  -- Zero derivative at boundaries
  | Periodic                                 -- Periodic boundaries
  | Outgoing                                 -- Radiation boundary conditions
  deriving (Show, Eq, Generic)

-- | Spectral decomposition result
data SpectralDecomposition = SpectralDecomposition
  { discreteEigenvalues :: [Eigenvalue]
  , discreteEigenfunctions :: [WaveFunction]
  , continuousThreshold :: Double
  , resonances :: [ComplexEigenvalue]
  } deriving (Show, Generic)

-- | Complex eigenvalue for resonances
data ComplexEigenvalue = ComplexEigenvalue
  { realPart :: Double        -- ε_R in the paper
  , decayWidth :: Double      -- Γ in the paper  
  } deriving (Show, Eq, Generic)

-- | Scattering amplitude
data ScatteringAmplitude = ScatteringAmplitude
  { momentum :: Double
  , amplitude :: Complex Double
  , angle :: Double
  } deriving (Show, Generic)

-- | Error correction parameters
data ErrorCorrectionParams = ErrorCorrectionParams
  { syndromeRate :: Double           -- Γ_syn in the paper
  , recoveryEnergy :: Energy         -- E_rec in the paper
  , decoherenceRate :: Double        -- Γ_decoherence
  , correctionTime :: Double         -- t_correction
  } deriving (Show, Generic)

-- | Emergent spacetime metric (simplified 2D for demonstration)
data SpacetimeMetric = SpacetimeMetric
  { backgroundMetric :: Matrix Double       -- η_μν (Minkowski)
  , perturbation :: Matrix Double           -- metric perturbation from spectrum
  } deriving (Show, Generic)

-- | Physical predictions from the spectral analysis
data PhysicalPredictions = PhysicalPredictions
  { modifiedDispersion :: Energy -> Double -> Energy    -- E(p) relation
  , gravitationalWaveFrequencies :: [Double]
  , blackHoleEntropy :: InformationDensity -> Double
  } deriving (Generic)

-- | Convert information density eigenvalue to energy
informationToEnergy :: InformationDensity -> Energy
informationToEnergy (InformationDensity epsilon) = 
  Energy (speedOfLight^2 * epsilon + quantumCorrection epsilon)
  where
    quantumCorrection eps = (eps^2) / (planckMass * speedOfLight^2)

-- | Calculate spectral gap
spectralGap :: [Eigenvalue] -> [Eigenvalue] -> Maybe Double
spectralGap discrete continuous = 
  case (maxDiscrete, minContinuous) of
    (Just maxD, Just minC) -> Just (minC - maxD)
    _ -> Nothing
  where
    maxDiscrete = case map extractReal discrete of
      [] -> Nothing
      xs -> Just (maximum xs)
    minContinuous = case map extractReal continuous of
      [] -> Nothing  
      xs -> Just (minimum xs)
    extractReal (RealEigenvalue x) = x
    extractReal (ComplexEigenvalue c) = realPart c

-- | Check stability condition for emergent spacetime
isSpacetimeStable :: SpectralDecomposition -> ErrorCorrectionParams -> Bool
isSpacetimeStable spectral errorParams =
  case spectralGap (discreteEigenvalues spectral) [] of
    Just gap -> gap > 0 && continuousThreshold spectral > criticalThreshold
    Nothing -> False
  where
    criticalThreshold = 
      (decoherenceRate errorParams) / 
      (speedOfLight^2 * correctionTime errorParams)

-- | Calculate information scattering cross-section
scatteringCrossSection :: ScatteringAmplitude -> Double
scatteringCrossSection scattering =
  (4 * pi * speedOfLight^2) / (momentum scattering) * imagPart (amplitude scattering)

-- | Extract decay width from complex eigenvalue  
getDecayWidth :: ComplexEigenvalue -> Double
getDecayWidth = decayWidth

-- | Semiclassical eigenvalue approximation
semiclassicalEigenvalue :: Int -> InformationDensity
semiclassicalEigenvalue n =
  InformationDensity $ (speedOfLight^2 / planckLength^2) * fromIntegral (n + 1/2)
  where
    planckLength = 1.0  -- Normalized Planck length