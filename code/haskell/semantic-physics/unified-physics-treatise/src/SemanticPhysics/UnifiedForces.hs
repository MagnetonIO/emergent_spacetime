{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module SemanticPhysics.UnifiedForces
    ( Force(..)
    , ForceType(..)
    , UnifiedField(..)
    , Particle(..)
    , computeForce
    , unifyForces
    , runningCoupling
    , computeScattering
    , forceFromConstraint
    ) where

import Data.Complex
import Numeric.LinearAlgebra
import SemanticPhysics.Core
import SemanticPhysics.Constraints
import SemanticPhysics.EmergentSpacetime

-- | Force representation
data Force = Force
    { forceType :: ForceType
    , forceMagnitude :: Double
    , forceDirection :: Vector Double
    , forceCoupling :: Double
    } deriving (Show, Eq)

-- | Types of fundamental forces
data ForceType 
    = Gravitational
    | Electromagnetic
    , Weak
    | Strong
    deriving (Show, Eq, Ord)

-- | Unified field representation
data UnifiedField = UnifiedField
    { ufInformationField :: InformationState
    , ufConstraints :: [(ConstraintType, Double)]
    , ufMetric :: EmergentMetric
    , ufGaugeField :: Matrix (Complex Double)
    }

-- | Particle representation
data Particle = Particle
    { particleMass :: Double
    , particleCharge :: Double
    , particleSpin :: Double
    , particleIsospin :: Vector Double
    , particleColor :: Vector (Complex Double)
    , particlePosition :: SpacetimePoint
    , particleMomentum :: Vector Double
    } deriving (Show)

-- | Compute force on particle from unified field
computeForce :: UnifiedField -> Particle -> ForceType -> Force
computeForce field particle Gravitational = computeGravitationalForce field particle
computeForce field particle Electromagnetic = computeElectromagneticForce field particle  
computeForce field particle Weak = computeWeakForce field particle
computeForce field particle Strong = computeStrongForce field particle

-- | Compute gravitational force (from metric curvature)
computeGravitationalForce :: UnifiedField -> Particle -> Force
computeGravitationalForce UnifiedField{..} particle = Force
    { forceType = Gravitational
    , forceMagnitude = magnitude
    , forceDirection = direction
    , forceCoupling = gNewton
    }
  where
    gNewton = 6.67430e-11
    einstein = computeEinsteinTensor ufMetric (particlePosition particle)
    stressEnergy = particleStressEnergy particle
    -- F = -m * Γ^μ_νρ v^ν v^ρ
    christoffel = emChristoffelSymbols ufMetric (particlePosition particle)
    velocity = fromList $ (1.0 : toList (scale (1.0 / particleMass particle) (particleMomentum particle)))
    
    acceleration = fromList
        [-sum [christoffel mu nu rho * (velocity ! nu) * (velocity ! rho)
              | nu <- [0..3], rho <- [0..3]]
        | mu <- [1..3]]  -- Spatial components only
    
    magnitude = particleMass particle * norm_2 acceleration
    direction = if magnitude > 0 then scale (1.0 / magnitude) acceleration else fromList [0,0,0]

-- | Compute electromagnetic force
computeElectromagneticForce :: UnifiedField -> Particle -> Force
computeElectromagneticForce UnifiedField{..} particle = Force
    { forceType = Electromagnetic
    , forceMagnitude = magnitude
    , forceDirection = direction  
    , forceCoupling = alpha
    }
  where
    alpha = 1.0 / 137.036  -- Fine structure constant
    charge = particleCharge particle
    fieldStrength = computeFieldStrengthAt ufGaugeField (particlePosition particle)
    -- F = q(E + v × B)
    electricField = extractElectricField fieldStrength
    magneticField = extractMagneticField fieldStrength
    velocity = scale (1.0 / particleMass particle) (particleMomentum particle)
    
    lorentzForce = scale charge $ electricField + crossProduct velocity magneticField
    magnitude = norm_2 lorentzForce
    direction = if magnitude > 0 then scale (1.0 / magnitude) lorentzForce else fromList [0,0,0]

-- | Compute weak force
computeWeakForce :: UnifiedField -> Particle -> Force
computeWeakForce UnifiedField{..} particle = Force
    { forceType = Weak
    , forceMagnitude = magnitude
    , forceDirection = direction
    , forceCoupling = gWeak
    }
  where
    gWeak = 0.65  -- Weak coupling constant
    mW = 80.379  -- W boson mass in GeV
    isospin = particleIsospin particle
    
    -- Weak force mediated by W/Z bosons
    range = hbar / (mW * c)  -- Yukawa potential range
    r = norm_2 $ spSpace $ particlePosition particle
    yukawaPotential = exp(-r / range) / r
    
    -- Force from isospin interaction
    weakField = scale yukawaPotential isospin
    magnitude = gWeak * norm_2 weakField
    direction = if magnitude > 0 then scale (1.0 / magnitude) weakField else fromList [0,0,0]
    
    hbar = 1.054571817e-34
    c = 299792458

-- | Compute strong force
computeStrongForce :: UnifiedField -> Particle -> Force
computeStrongForce UnifiedField{..} particle = Force
    { forceType = Strong
    , forceMagnitude = magnitude
    , forceDirection = direction
    , forceCoupling = alphaS
    }
  where
    alphaS = computeStrongCoupling (norm_2 $ particleMomentum particle)
    color = particleColor particle
    
    -- Confinement potential V = σr for large r
    sigma = 0.18  -- String tension in GeV²
    r = norm_2 $ spSpace $ particlePosition particle
    
    -- Color force
    colorField = computeColorField ufInformationField (particlePosition particle)
    colorForce = matrixVectorProduct colorField color
    
    -- Combined QCD force
    confinementForce = scale (sigma * r) (normalize $ realPart <$> colorForce)
    magnitude = norm_2 confinementForce
    direction = if magnitude > 0 then scale (1.0 / magnitude) confinementForce else fromList [0,0,0]

-- | Unify forces at given energy scale
unifyForces :: Double -> [(ForceType, Double)]
unifyForces energy = 
    let couplings = map (\ft -> (ft, runningCoupling ft energy)) [Gravitational, Electromagnetic, Weak, Strong]
    in couplings

-- | Running coupling constants
runningCoupling :: ForceType -> Double -> Double
runningCoupling Gravitational energy = 
    -- Gravity doesn't run at one-loop
    6.67430e-11
    
runningCoupling Electromagnetic energy = 
    let alpha0 = 1.0 / 137.036
        mZ = 91.2  -- Z boson mass
        beta = 1.0 / (6.0 * pi)  -- QED beta function
    in alpha0 / (1.0 - beta * alpha0 * log(energy / mZ))
    
runningCoupling Weak energy = 
    let g0 = 0.65
        mZ = 91.2
        beta = -19.0 / (6.0 * pi)  -- Weak beta function
    in g0 / (1.0 + beta * g0 * log(energy / mZ))
    
runningCoupling Strong energy = 
    let lambda = 0.2  -- QCD scale
        beta0 = 11.0 - 2.0/3.0 * 6.0  -- nf = 6 flavors
    in 4.0 * pi / (beta0 * log(energy / lambda))

-- | Compute scattering amplitude
computeScattering :: UnifiedField -> Particle -> Particle -> Complex Double
computeScattering field p1 p2 = 
    let s = invariantMass p1 p2
        forces = [computeForce field p1 ft | ft <- [Gravitational, Electromagnetic, Weak, Strong]]
        amplitudes = map (scatteringAmplitude s) forces
    in sum amplitudes

-- | Scattering amplitude for given force
scatteringAmplitude :: Double -> Force -> Complex Double
scatteringAmplitude s Force{..} = 
    let propagator = case forceType of
            Gravitational -> 1.0 / s  -- Massless graviton
            Electromagnetic -> 1.0 / s  -- Massless photon
            Weak -> 1.0 / (s - 80.379^2)  -- Massive W boson
            Strong -> 1.0 / s  -- Massless gluon
    in (forceCoupling :+ 0) * propagator

-- | Force from constraint gradient
forceFromConstraint :: ConstraintType -> InformationState -> Force
forceFromConstraint cType state = Force
    { forceType = constraintToForceType cType
    , forceMagnitude = magnitude
    , forceDirection = direction
    , forceCoupling = constraintCoupling cType
    }
  where
    gradient = constraintGradient cType state
    magnitude = norm_2 gradient
    direction = if magnitude > 0 then scale (1.0 / magnitude) gradient else fromList [0,0,0]

-- Helper functions

-- | Map constraint type to force type
constraintToForceType :: ConstraintType -> ForceType
constraintToForceType Geometric = Gravitational
constraintToForceType Gauge = Electromagnetic
constraintToForceType SymmetryBreaking = Weak
constraintToForceType Confinement = Strong

-- | Constraint coupling constants
constraintCoupling :: ConstraintType -> Double
constraintCoupling Geometric = 6.67430e-11
constraintCoupling Gauge = 1.0 / 137.036
constraintCoupling SymmetryBreaking = 0.65
constraintCoupling Confinement = 0.3  -- αs at low energy

-- | Particle stress-energy tensor
particleStressEnergy :: Particle -> Matrix Double
particleStressEnergy particle = 
    let m = particleMass particle
        p = particleMomentum particle
        E = sqrt $ m^2 + norm_2 p ^ 2
        fourMomentum = fromList $ E : toList p
    in scale (1.0 / E) $ outer fourMomentum fourMomentum

-- | Compute field strength at point
computeFieldStrengthAt :: Matrix (Complex Double) -> SpacetimePoint -> Matrix (Complex Double)
computeFieldStrengthAt gaugeField point = gaugeField  -- Simplified

-- | Extract electric field (spatial components of F^{0i})
extractElectricField :: Matrix (Complex Double) -> Vector Double
extractElectricField fieldStrength = 
    fromList [realPart $ fieldStrength ! (0, i) | i <- [1..3]]

-- | Extract magnetic field (spatial components of F^{ij})
extractMagneticField :: Matrix (Complex Double) -> Vector Double
extractMagneticField fieldStrength = 
    fromList [realPart $ fieldStrength ! (2, 3),  -- Bx = F23
              realPart $ fieldStrength ! (3, 1),  -- By = F31
              realPart $ fieldStrength ! (1, 2)]  -- Bz = F12

-- | Cross product for 3-vectors
crossProduct :: Vector Double -> Vector Double -> Vector Double
crossProduct v1 v2 = fromList
    [v1 ! 1 * v2 ! 2 - v1 ! 2 * v2 ! 1,
     v1 ! 2 * v2 ! 0 - v1 ! 0 * v2 ! 2,
     v1 ! 0 * v2 ! 1 - v1 ! 1 * v2 ! 0]

-- | Normalize vector
normalize :: Vector Double -> Vector Double
normalize v = let n = norm_2 v in if n > 0 then scale (1.0 / n) v else v

-- | Strong coupling at energy scale
computeStrongCoupling :: Double -> Double
computeStrongCoupling energy = runningCoupling Strong energy

-- | Compute color field
computeColorField :: InformationState -> SpacetimePoint -> Matrix (Complex Double)
computeColorField state point = isStructure state  -- Simplified

-- | Matrix-vector product for complex
matrixVectorProduct :: Matrix (Complex Double) -> Vector (Complex Double) -> Vector (Complex Double)
matrixVectorProduct m v = m #> v

-- | Invariant mass
invariantMass :: Particle -> Particle -> Double
invariantMass p1 p2 = 
    let e1 = sqrt $ particleMass p1 ^ 2 + norm_2 (particleMomentum p1) ^ 2
        e2 = sqrt $ particleMass p2 ^ 2 + norm_2 (particleMomentum p2) ^ 2
        p12 = particleMomentum p1 + particleMomentum p2
    in sqrt $ (e1 + e2)^2 - norm_2 p12 ^ 2