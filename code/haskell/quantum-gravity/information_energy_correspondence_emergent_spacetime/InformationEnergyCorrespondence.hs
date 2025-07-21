{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module InformationEnergyCorrespondence where

import Data.Complex
import GHC.Generics
import Numeric.LinearAlgebra

-- | Core types for the information-energy correspondence framework
data PhysicalConstants = PhysicalConstants
  { planckLength :: Double  -- l_P
  , speedOfLight :: Double  -- c
  , gravitationalConstant :: Double  -- G
  , reducedPlanck :: Double  -- ℏ
  } deriving (Show, Eq, Generic)

-- | Standard physical constants in SI units
standardConstants :: PhysicalConstants
standardConstants = PhysicalConstants
  { planckLength = 1.616255e-35  -- meters
  , speedOfLight = 299792458     -- m/s
  , gravitationalConstant = 6.67430e-11  -- m³/(kg·s²)
  , reducedPlanck = 1.054571817e-34  -- J·s
  }

-- | Natural units where c = ℏ = G = 1
naturalUnits :: PhysicalConstants
naturalUnits = PhysicalConstants
  { planckLength = 1.0
  , speedOfLight = 1.0
  , gravitationalConstant = 1.0
  , reducedPlanck = 1.0
  }

-- | Information content of a physical system
newtype InformationContent = InformationContent Double
  deriving (Show, Eq, Ord)

-- | Energy density
newtype EnergyDensity = EnergyDensity Double
  deriving (Show, Eq, Ord)

-- | Characteristic length scale
newtype LengthScale = LengthScale Double
  deriving (Show, Eq, Ord)

-- | Critical exponents for the scaling relation
data CriticalExponents = CriticalExponents
  { alphaExponent :: Double  -- Energy exponent (3/4)
  , betaExponent :: Double   -- Length exponent (2)
  } deriving (Show, Eq)

-- | Standard critical exponents derived in the paper
standardExponents :: CriticalExponents
standardExponents = CriticalExponents
  { alphaExponent = 0.75  -- 3/4
  , betaExponent = 2.0
  }

-- | Main correspondence relation: I = A · E^α · L^β
informationEnergyCorrespondence :: 
  Double ->  -- Dimensionless constant A
  CriticalExponents ->
  EnergyDensity ->
  LengthScale ->
  InformationContent
informationEnergyCorrespondence a CriticalExponents{..} (EnergyDensity e) (LengthScale l) =
  InformationContent $ a * (e ** alphaExponent) * (l ** betaExponent)

-- | Holographic information bound for a spherical region
holographicBound :: PhysicalConstants -> LengthScale -> InformationContent
holographicBound PhysicalConstants{..} (LengthScale radius) =
  InformationContent $ (4 * pi * radius^2) / (4 * planckLength^2)

-- | Energy density from holographic principle
holographicEnergyDensity :: PhysicalConstants -> LengthScale -> EnergyDensity
holographicEnergyDensity PhysicalConstants{..} (LengthScale l) =
  EnergyDensity $ (3 * speedOfLight^4) / (32 * pi * gravitationalConstant * l^2)

-- | Effective gravitational coupling in emergent spacetime
effectiveGravitationalCoupling :: 
  PhysicalConstants ->
  InformationContent ->  -- Local information content
  Double  -- Effective G
effectiveGravitationalCoupling PhysicalConstants{..} (InformationContent i_local) =
  planckLength^2 / i_local

-- | Vacuum energy density from information content
vacuumEnergyDensity :: PhysicalConstants -> InformationContent -> EnergyDensity
vacuumEnergyDensity PhysicalConstants{..} (InformationContent i_vac) =
  EnergyDensity $ (i_vac ** (4/3)) / planckLength^2

-- | Decoherence time scale
decoherenceTime :: InformationContent -> Double
decoherenceTime (InformationContent i) = 1.0 / sqrt i

-- | Decoherence time with full scaling law
decoherenceTimeScaling :: EnergyDensity -> LengthScale -> Double
decoherenceTimeScaling (EnergyDensity e) (LengthScale l) =
  1.0 / (e ** (3/8) * l)

-- | Information density distribution
type InformationDensity = Vector Double

-- | Pre-geometric coordinates
type PreGeometricCoords = Vector Double

-- | Emergent metric tensor components
data MetricTensor = MetricTensor
  { g00 :: Double
  , g01 :: Double
  , g02 :: Double
  , g03 :: Double
  , g11 :: Double
  , g12 :: Double
  , g13 :: Double
  , g22 :: Double
  , g23 :: Double
  , g33 :: Double
  } deriving (Show, Eq)

-- | Minkowski metric in standard coordinates
minkowskiMetric :: MetricTensor
minkowskiMetric = MetricTensor
  { g00 = -1.0
  , g01 = 0.0
  , g02 = 0.0
  , g03 = 0.0
  , g11 = 1.0
  , g12 = 0.0
  , g13 = 0.0
  , g22 = 1.0
  , g23 = 0.0
  , g33 = 1.0
  }

-- | Convert metric tensor to matrix representation
metricToMatrix :: MetricTensor -> Matrix Double
metricToMatrix MetricTensor{..} = (4><4)
  [ g00, g01, g02, g03
  , g01, g11, g12, g13
  , g02, g12, g22, g23
  , g03, g13, g23, g33
  ]

-- | Information field configuration
data InformationField = InformationField
  { fieldAmplitude :: PreGeometricCoords -> Double
  , fieldGradient :: PreGeometricCoords -> Vector Double
  , fieldLaplacian :: PreGeometricCoords -> Double
  }

-- | Compute emergent metric from information density
emergentMetric :: 
  PhysicalConstants ->
  InformationContent ->  -- Reference information density I_0
  InformationField ->
  PreGeometricCoords ->
  MetricTensor
emergentMetric PhysicalConstants{..} (InformationContent i0) field coords =
  let grad = fieldGradient field coords
      laplacian = fieldLaplacian field coords
      factor = planckLength^2 / i0
      -- Compute metric perturbations
      h_mu_nu = outerProduct grad grad - 0.5 * scalar laplacian * ident 4
  in addMetrics minkowskiMetric (scaleMetric factor (matrixToMetric h_mu_nu))
  where
    outerProduct :: Vector Double -> Vector Double -> Matrix Double
    outerProduct v1 v2 = asColumn v1 <> asRow v2
    
    scalar :: Double -> Matrix Double
    scalar x = (1><1) [x]
    
    matrixToMetric :: Matrix Double -> MetricTensor
    matrixToMetric m = MetricTensor
      { g00 = m `atIndex` (0,0)
      , g01 = m `atIndex` (0,1)
      , g02 = m `atIndex` (0,2)
      , g03 = m `atIndex` (0,3)
      , g11 = m `atIndex` (1,1)
      , g12 = m `atIndex` (1,2)
      , g13 = m `atIndex` (1,3)
      , g22 = m `atIndex` (2,2)
      , g23 = m `atIndex` (2,3)
      , g33 = m `atIndex` (3,3)
      }
    
    addMetrics :: MetricTensor -> MetricTensor -> MetricTensor
    addMetrics m1 m2 = MetricTensor
      { g00 = g00 m1 + g00 m2
      , g01 = g01 m1 + g01 m2
      , g02 = g02 m1 + g02 m2
      , g03 = g03 m1 + g03 m2
      , g11 = g11 m1 + g11 m2
      , g12 = g12 m1 + g12 m2
      , g13 = g13 m1 + g13 m2
      , g22 = g22 m1 + g22 m2
      , g23 = g23 m1 + g23 m2
      , g33 = g33 m1 + g33 m2
      }
    
    scaleMetric :: Double -> MetricTensor -> MetricTensor
    scaleMetric s m = MetricTensor
      { g00 = s * g00 m
      , g01 = s * g01 m
      , g02 = s * g02 m
      , g03 = s * g03 m
      , g11 = s * g11 m
      , g12 = s * g12 m
      , g13 = s * g13 m
      , g22 = s * g22 m
      , g23 = s * g23 m
      , g33 = s * g33 m
      }