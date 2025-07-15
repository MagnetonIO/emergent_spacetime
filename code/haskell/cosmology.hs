{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Cosmology
-- Description : Cosmological implications of information-theoretic gravity
-- Authors     : Matthew Long, ChatGPT 4o, Claude Sonnet 4
--
-- This module explores cosmological consequences of emergent spacetime,
-- including information-driven inflation and dark energy.

module Cosmology where

import qualified Data.Vector as V
import qualified Data.Matrix as M
import Data.Complex
import Control.Monad
import Numeric.Integration.TanhSinh
import Numeric.GSL.ODE

-- * Cosmological Models

-- | FLRW universe with information corrections
data Universe = Universe {
  scaleFactor :: Double -> Double,              -- a(t)
  hubbleParameter :: Double -> Double,          -- H(t) = ȧ/a
  densityParameter :: Double -> Double,         -- Ω(t)
  informationDensity :: Double -> Double,       -- ρ_info(t)
  cosmologicalConstant :: Double,              -- Λ
  curvature :: CurvatureType
}

data CurvatureType = Flat | Closed | Open deriving (Show, Eq)

-- | Energy components in the universe
data EnergyComponents = EnergyComponents {
  radiation :: Double,          -- ρ_r
  matter :: Double,            -- ρ_m
  darkEnergy :: Double,        -- ρ_DE
  information :: Double        -- ρ_info
} deriving (Show)

-- | Create standard ΛCDM universe
lambdaCDM :: Double -> Double -> Double -> Universe
lambdaCDM h0 omega_m omega_lambda = Universe {
  scaleFactor = \t -> (omega_m/omega_lambda)^(1/3) * sinh(3/2 * h0 * sqrt(omega_lambda) * t)^(2/3),
  hubbleParameter = \t -> h0 * sqrt(omega_m * a^(-3) + omega_lambda)
    where a = (omega_m/omega_lambda)^(1/3) * sinh(3/2 * h0 * sqrt(omega_lambda) * t)^(2/3),
  densityParameter = \t -> omega_m * ((h0/hubble_t)^2) * (a0/a_t)^3
    where 
      hubble_t = h0 * sqrt(omega_m * a_t^(-3) + omega_lambda)
      a_t = (omega_m/omega_lambda)^(1/3) * sinh(3/2 * h0 * sqrt(omega_lambda) * t)^(2/3)
      a0 = 1,
  informationDensity = \_ -> 0,  -- No information component in standard model
  cosmologicalConstant = 3 * h0^2 * omega_lambda,
  curvature = Flat
}

-- * Information-Driven Inflation

-- | Inflationary epoch driven by information processing
data Inflation = Inflation {
  startTime :: Double,
  endTime :: Double,
  efoldings :: Double,
  reheatingTemp :: Double,
  informationProduction :: Double -> Double
}

-- | Information-theoretic inflaton potential
inflatonPotential :: Double -> Double -> Double
inflatonPotential phi m =
  let v0 = m^2 * m_pl^2  -- Potential scale
      -- Information-motivated potential: V(φ) = V₀(1 - exp(-φ/M))
  in v0 * (1 - exp(-phi / m_pl))
  where
    m_pl = 1.22e19  -- Planck mass in GeV

-- | Solve inflationary dynamics
solveInflation :: Double -> Double -> Inflation
solveInflation phi0 m =
  let -- Slow-roll parameters
      epsilon = slowRollEpsilon phi0 m
      eta = slowRollEta phi0 m
      -- Number of e-foldings
      n_e = computeEfoldings phi0 m
      -- Duration
      dt = n_e / hubbleInflation phi0 m
  in Inflation {
    startTime = 0,
    endTime = dt,
    efoldings = n_e,
    reheatingTemp = computeReheating phi0 m,
    informationProduction = \t -> informationRate phi0 m t
  }

-- | Slow-roll parameter ε
slowRollEpsilon :: Double -> Double -> Double
slowRollEpsilon phi m =
  let v = inflatonPotential phi m
      dv = m * m_pl * exp(-phi/m_pl)  -- dV/dφ
      m_pl = 1.22e19
  in 0.5 * (m_pl * dv / v)^2

-- | Slow-roll parameter η
slowRollEta :: Double -> Double -> Double
slowRollEta phi m =
  let v = inflatonPotential phi m
      d2v = -(m/m_pl) * m * m_pl * exp(-phi/m_pl)  -- d²V/dφ²
      m_pl = 1.22e19
  in m_pl^2 * d2v / v

-- | Hubble parameter during inflation
hubbleInflation :: Double -> Double -> Double
hubbleInflation phi m =
  let v = inflatonPotential phi m
      m_pl = 1.22e19
  in sqrt(v / (3 * m_pl^2))

-- | Number of e-foldings
computeEfoldings :: Double -> Double -> Double
computeEfoldings phi0 m =
  let phi_end = m_pl * log(2)  -- End when ε = 1
      m_pl = 1.22e19
      integrand phi = inflatonPotential phi m / (m * m_pl * exp(-phi/m_pl))
      (result, _) = absolute 1e-6 $ integrate integrand phi_end phi0
  in result / m_pl^2

-- | Reheating temperature
computeReheating :: Double -> Double -> Double
computeReheating phi0 m =
  let gamma = 0.1 * m  -- Decay width
      m_pl = 1.22e19
  in 0.1 * sqrt(gamma * m_pl)

-- | Information production rate
informationRate :: Double -> Double -> Double -> Double
informationRate phi0 m t =
  let h = hubbleInflation phi0 m
      -- Information production: dS/dt ∝ H³
  in h^3 * t

-- * Dark Energy as Entanglement

-- | Dark energy from long-range entanglement
darkEnergyDensity :: Double -> Double -> Double
darkEnergyDensity entropy volume =
  let l_pl = 1.616e-35  -- Planck length
      -- ρ_DE = (entropy/volume) * (energy scale)
      energyScale = 1 / l_pl  -- Planck energy
  in (entropy / volume) * energyScale * l_pl^3

-- | Entanglement entropy of cosmological horizon
horizonEntropy :: Double -> Double
horizonEntropy hubble =
  let r_h = c / hubble  -- Hubble radius
      a_h = 4 * pi * r_h^2  -- Horizon area
      l_pl = 1.616e-35
  in a_h / (4 * l_pl^2)
  where
    c = 2.998e8

-- | Vacuum entanglement structure
vacuumEntanglement :: Double -> Double -> Double
vacuumEntanglement scale cutoff =
  let -- Entanglement entropy: S ∝ Volume * (Λ_UV/scale)^(d-1)
      d = 4  -- Spacetime dimensions
  in scale^3 * (cutoff/scale)^(d-1)

-- * Evolution Equations

-- | Friedmann equations with information corrections
friedmannEquations :: EnergyComponents -> Double -> (Double, Double)
friedmannEquations comps a =
  let rho_total = totalDensity comps a
      p_total = totalPressure comps a
      -- H² = (8πG/3)ρ
      h_squared = (8 * pi * g_newton / 3) * rho_total
      -- ä/a = -(4πG/3)(ρ + 3p)
      a_ddot_over_a = -(4 * pi * g_newton / 3) * (rho_total + 3 * p_total)
  in (sqrt h_squared, a_ddot_over_a)
  where
    g_newton = 6.674e-11

-- | Total energy density
totalDensity :: EnergyComponents -> Double -> Double
totalDensity EnergyComponents{..} a =
  radiation / a^4 + matter / a^3 + darkEnergy + information / a^2

-- | Total pressure
totalPressure :: EnergyComponents -> Double -> Double
totalPressure EnergyComponents{..} a =
  radiation / (3 * a^4) - darkEnergy + information / (3 * a^2)

-- | Information-modified Raychaudhuri equation
raychaudhuriEquation :: Double -> Double -> Double -> Double
raychaudhuriEquation theta sigma omega =
  let -- dθ/dτ = -θ²/3 - σ² + ω² - R_μν u^μ u^ν
      expansion = -theta^2 / 3
      shear = -sigma^2
      rotation = omega^2
      -- Information correction to Ricci tensor
      infoCorrection = -0.1 * theta  -- Placeholder
  in expansion + shear + rotation + infoCorrection

-- * Structure Formation

-- | Growth of density perturbations
data Perturbation = Perturbation {
  wavenumber :: Double,           -- k
  amplitude :: Double -> Double,  -- δ(t)
  growthRate :: Double -> Double  -- f(t) = d ln δ/d ln a
}

-- | Linear growth equation with information
linearGrowth :: Universe -> Double -> Perturbation
linearGrowth uni k = Perturbation {
  wavenumber = k,
  amplitude = \t -> d_plus uni t,
  growthRate = \t -> f_growth uni t
}

-- | Growing mode solution D₊(t)
d_plus :: Universe -> Double -> Double
d_plus uni t =
  let a = scaleFactor uni t
      h = hubbleParameter uni t
      omega_m = densityParameter uni t
      -- D₊ ∝ H(a) ∫ da' / (a' H(a'))³
      integrand a' = 1 / (a' * (hubbleParameter uni (time_of_a uni a'))^3)
      (result, _) = absolute 1e-6 $ integrate integrand 0 a
  in h * result
  where
    time_of_a uni a = 1.0  -- Placeholder: inverse of a(t)

-- | Growth rate f = d ln D/d ln a
f_growth :: Universe -> Double -> Double  
f_growth uni t =
  let omega_m = densityParameter uni t
      -- Approximation: f ≈ Ω_m^γ with γ ≈ 0.545
      gamma = 0.545 + 0.05 * informationDensity uni t  -- Information correction
  in omega_m ** gamma

-- | Information-theoretic Jeans length
jeansLength :: Double -> Double -> Double
jeansLength infoRate soundSpeed =
  let -- Modified Jeans length includes information pressure
      standard = 2 * pi * soundSpeed / sqrt(4 * pi * g_newton * rho_matter)
      infoCorrection = sqrt(infoRate / rho_matter)
      rho_matter = 1e-26  -- kg/m³ (typical)
  in standard * (1 + infoCorrection)
  where
    g_newton = 6.674e-11

-- * Primordial Fluctuations

-- | Quantum fluctuations during inflation
primordialSpectrum :: Inflation -> Double -> Double
primordialSpectrum inf k =
  let h_inf = 1e13  -- Hubble during inflation (GeV)
      m_pl = 1.22e19
      -- Power spectrum: P(k) = (H²/2π)² / (2ε)
      epsilon = 0.01  -- Slow-roll parameter
  in (h_inf / (2 * pi))^2 / (2 * epsilon * m_pl^2)

-- | Spectral index
spectralIndex :: Double -> Double -> Double
spectralIndex epsilon eta = 1 - 6*epsilon + 2*eta

-- | Tensor-to-scalar ratio
tensorToScalar :: Double -> Double
tensorToScalar epsilon = 16 * epsilon

-- * Observable Signatures

-- | CMB temperature anisotropies
cmbAnisotropy :: Double -> Double -> Double
cmbAnisotropy l infoParam =
  let -- C_l with information corrections
      standard = 1e-10 * (l*(l+1))^(-0.8)  -- Standard CDM spectrum
      infoBoost = 1 + 0.1 * infoParam * exp(-l/1000)
  in standard * infoBoost

-- | Baryon acoustic oscillations
baoScale :: Universe -> Double -> Double
baoScale uni z =
  let -- Sound horizon at drag epoch
      standard = 147  -- Mpc
      -- Information correction
      infoCorrection = 1 + 0.01 * informationDensity uni (time_of_z z)
  in standard * infoCorrection
  where
    time_of_z z = log(1 + z) / 70  -- Rough approximation

-- | Integrated Sachs-Wolfe effect
iswEffect :: Universe -> Double -> Double -> Double
iswEffect uni z k =
  let phi_dot = gravitationalPotentialEvolution uni z k
      -- ISW: ΔT/T = 2 ∫ Φ̇ dτ
  in 2 * phi_dot * conformalTime uni z

-- | Gravitational potential evolution
gravitationalPotentialEvolution :: Universe -> Double -> Double -> Double
gravitationalPotentialEvolution uni z k =
  let a = 1 / (1 + z)
      h = hubbleParameter uni (time_of_z z)
      -- Φ̇ = -H·Φ·(1 + f) for growing modes
      f = f_growth uni (time_of_z z)
  in -h * (1 + f) / a
  where
    time_of_z z = log(1 + z) / 70

-- | Conformal time
conformalTime :: Universe -> Double -> Double
conformalTime uni z =
  let integrand z' = 1 / ((1 + z') * hubbleParameter uni (time_of_z z'))
      (result, _) = absolute 1e-6 $ integrate integrand 0 z
  in result / c
  where
    c = 2.998e8
    time_of_z z = log(1 + z) / 70

-- * Constants
pi :: Double
pi = 3.14159265358979323846

c :: Double
c = 2.998e8  -- m/s