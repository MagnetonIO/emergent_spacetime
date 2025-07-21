{-# LANGUAGE RecordWildCards #-}

module CosmologicalApplications where

import InformationEnergyCorrespondence
import Numeric.LinearAlgebra
import Data.List (foldl')

-- | Scale factor evolution in the early universe
data ScaleFactor = ScaleFactor
  { time :: Double
  , value :: Double
  } deriving (Show, Eq)

-- | Information content evolution function
type InformationEvolution = Double -> InformationContent

-- | Compute scale factor evolution from information dynamics
-- a(t) ∝ exp[∫√(I(t')/3) dt']
scaleFactorEvolution :: 
  InformationEvolution ->
  Double ->  -- Initial time
  Double ->  -- Final time
  Int ->     -- Number of integration steps
  [ScaleFactor]
scaleFactorEvolution infoEvol t0 tf nSteps =
  let dt = (tf - t0) / fromIntegral nSteps
      times = [t0 + fromIntegral i * dt | i <- [0..nSteps]]
      integrand t = case infoEvol t of
        InformationContent i -> sqrt (i / 3.0)
      -- Numerical integration using trapezoidal rule
      integrate ts = dt * sum [0.5 * (integrand t1 + integrand t2) 
                               | (t1, t2) <- zip ts (tail ts)]
      scaleFactor t = exp $ integrate [t0, t0 + dt .. t]
  in [ScaleFactor t (scaleFactor t) | t <- times]

-- | Information density perturbation
data InformationPerturbation = InformationPerturbation
  { perturbationAmplitude :: Double
  , perturbationWavenumber :: Double
  , perturbationPhase :: Double
  } deriving (Show, Eq)

-- | Hubble parameter
newtype HubbleParameter = HubbleParameter Double
  deriving (Show, Eq)

-- | Structure formation equation
-- d²δ_I/dt² + 2H(dδ_I/dt) = 4πGρ_eff δ_I
structureFormation ::
  PhysicalConstants ->
  HubbleParameter ->
  EnergyDensity ->     -- Effective energy density
  InformationPerturbation ->
  Double ->            -- Time
  Double               -- Perturbation growth rate
structureFormation PhysicalConstants{..} (HubbleParameter h) (EnergyDensity rho_eff) 
                  InformationPerturbation{..} t =
  let omega = sqrt $ 4 * pi * gravitationalConstant * rho_eff
      damping = 2 * h
      -- Solution for growing mode
      discriminant = damping^2 - 4 * omega^2
  in if discriminant >= 0
     then perturbationAmplitude * exp(-damping * t / 2) * 
          cosh(sqrt discriminant * t / 2)
     else perturbationAmplitude * exp(-damping * t / 2) * 
          cos(sqrt (-discriminant) * t / 2)

-- | Dark matter as concentrated information regions
data DarkMatterHalo = DarkMatterHalo
  { haloRadius :: LengthScale
  , haloInformationContent :: InformationContent
  , haloPosition :: Vector Double  -- 3D position
  } deriving (Show, Eq)

-- | Compute gravitational effect of dark matter halo
darkMatterGravity ::
  PhysicalConstants ->
  DarkMatterHalo ->
  Vector Double ->     -- Test particle position
  Vector Double        -- Gravitational acceleration
darkMatterGravity constants DarkMatterHalo{..} testPos =
  let r_vec = testPos - haloPosition
      r = norm_2 r_vec
      (LengthScale r_halo) = haloRadius
      (InformationContent i_halo) = haloInformationContent
      -- Effective mass from information content
      m_eff = i_halo * constants.planckLength / constants.speedOfLight^2
      -- Newtonian approximation for simplicity
      g_magnitude = constants.gravitationalConstant * m_eff / r^2
  in if r > r_halo
     then scale (-g_magnitude / r) r_vec
     else -- Inside the halo, use uniform density approximation
          scale (-constants.gravitationalConstant * m_eff * r / r_halo^3) r_vec

-- | Dark energy from background information
darkEnergyDensity ::
  PhysicalConstants ->
  InformationContent ->  -- Background information content
  EnergyDensity
darkEnergyDensity constants i_background =
  vacuumEnergyDensity constants i_background

-- | Modified Friedmann equation with information dynamics
modifiedFriedmann ::
  PhysicalConstants ->
  InformationContent ->  -- Total information content
  LengthScale ->         -- Universe scale
  HubbleParameter
modifiedFriedmann constants i_total (LengthScale a) =
  let (EnergyDensity rho) = holographicEnergyDensity constants (LengthScale a)
      -- Include information correction
      (InformationContent i) = i_total
      correction = sqrt(i / 3.0) / a
  in HubbleParameter $ sqrt(8 * pi * constants.gravitationalConstant * rho / 3) + correction

-- | CMB temperature fluctuations from primordial information structure
data CMBFluctuation = CMBFluctuation
  { angularPosition :: (Double, Double)  -- (theta, phi)
  , temperatureDeviation :: Double       -- ΔT/T
  } deriving (Show, Eq)

-- | Generate CMB fluctuations from information field
generateCMBFluctuations ::
  InformationField ->
  Int ->               -- Number of angular samples
  [CMBFluctuation]
generateCMBFluctuations field nSamples =
  let angles = [(theta, phi) | 
                theta <- [pi * fromIntegral i / fromIntegral nSamples | i <- [0..nSamples]],
                phi <- [2 * pi * fromIntegral j / fromIntegral nSamples | j <- [0..nSamples]]]
      -- Map angular position to pre-geometric coordinates
      angleToCoords (theta, phi) = fromList [cos phi * sin theta, 
                                            sin phi * sin theta, 
                                            cos theta, 
                                            0.0]  -- Time component
      -- Temperature deviation proportional to information density
      tempDeviation pos = 
        let coords = angleToCoords pos
            info = fieldAmplitude field coords
        in (info - 1.0) * 1e-5  -- Normalized to observed scale
  in [CMBFluctuation pos (tempDeviation pos) | pos <- angles]

-- | Inflation parameters from information phase transition
data InflationParameters = InflationParameters
  { efoldings :: Double           -- Number of e-foldings
  , spectralIndex :: Double       -- n_s
  , tensorToScalarRatio :: Double -- r
  } deriving (Show, Eq)

-- | Calculate inflation parameters from information dynamics
calculateInflationParameters ::
  InformationEvolution ->
  Double ->  -- Initial time
  Double ->  -- Final time
  InflationParameters
calculateInflationParameters infoEvol t_i t_f =
  let -- Number of e-foldings
      n_e = integrate (\t -> case infoEvol t of
                               InformationContent i -> sqrt(i / 3.0)) t_i t_f
      -- Spectral index (simplified calculation)
      n_s = 1.0 - 2.0 / n_e
      -- Tensor-to-scalar ratio
      r = 16.0 / n_e
  in InflationParameters n_e n_s r
  where
    integrate f a b = 
      let n = 1000
          h = (b - a) / fromIntegral n
          xs = [a + fromIntegral i * h | i <- [0..n]]
      in h * sum [f x | x <- xs]

-- | Modified dispersion relation for gravitational waves
gravitationalWaveDispersion ::
  PhysicalConstants ->
  InformationContent ->  -- Local information density
  Double ->              -- Frequency
  Double                 -- Phase velocity
gravitationalWaveDispersion PhysicalConstants{..} (InformationContent i_local) omega =
  let v0 = speedOfLight
      -- Information-induced correction
      correction = 1.0 - (planckLength * omega)^2 / i_local
  in v0 * sqrt correction