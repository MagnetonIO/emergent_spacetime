{-# LANGUAGE RecordWildCards #-}

module ScatteringTheory where

import SpectralTypes
import InformationOperator
import SpectralSolver
import Numeric.LinearAlgebra hiding (Complex)
import qualified Numeric.LinearAlgebra as LA
import Data.Complex
import Control.Monad (forM)

-- | Calculate scattering amplitude for information patterns
-- f(θ) in the paper - equation for asymptotic form
calculateScatteringAmplitude :: InformationOperator -> Double -> Double -> ScatteringAmplitude
calculateScatteringAmplitude op energy angle =
  ScatteringAmplitude
    { momentum = sqrt (energy / speedOfLight^2)
    , amplitude = computeAmplitude op energy angle
    , angle = angle
    }
  where
    computeAmplitude op e theta =
      -- Born approximation for scattering amplitude
      let k = sqrt (e / speedOfLight^2)
          q = 2 * k * sin (theta / 2)  -- Momentum transfer
      in bornApproximation op q

-- | Born approximation for scattering
bornApproximation :: InformationOperator -> Double -> Complex Double
bornApproximation InformationOperator{..} momentumTransfer =
  -- Simplified Born approximation
  -- f(q) = -2πm/ℏ² ∫ V(r) e^(iq·r) d³r
  let matrixElement = trace operatorMatrix / fromIntegral (rows operatorMatrix)
  in (-2 * pi :+ 0) * matrixElement * exp (0 :+ (-momentumTransfer^2 / 2))

-- | Total scattering cross-section using optical theorem
totalCrossSection :: ScatteringAmplitude -> Double
totalCrossSection ScatteringAmplitude{..} =
  (4 * pi * speedOfLight^2) / momentum * imagPart amplitude

-- | Differential cross-section
differentialCrossSection :: ScatteringAmplitude -> Double
differentialCrossSection ScatteringAmplitude{..} =
  magnitude amplitude ^ 2

-- | Phase shift analysis for scattering
phaseShift :: InformationOperator -> Double -> Int -> Double
phaseShift op energy angularMomentum =
  -- δ_l(k) phase shift for partial wave l
  let k = sqrt (energy / speedOfLight^2)
      -- Simplified phase shift calculation
  in atan (angularMomentum / (k * fromIntegral (rows (operatorMatrix op))))

-- | Partial wave decomposition
partialWaveAmplitude :: InformationOperator -> Double -> Int -> Complex Double
partialWaveAmplitude op energy l =
  let k = sqrt (energy / speedOfLight^2)
      delta = phaseShift op energy l
  in (exp (2 * 0 :+ delta) - 1) / (2 * 0 :+ k)

-- | S-matrix element
sMatrixElement :: InformationOperator -> Double -> Int -> Complex Double
sMatrixElement op energy l =
  exp (2 * 0 :+ phaseShift op energy l)

-- | Resonance width from poles in S-matrix
resonanceWidth :: ComplexEigenvalue -> Double
resonanceWidth = decayWidth

-- | Breit-Wigner resonance profile
breitWigner :: Double -> ComplexEigenvalue -> Double
breitWigner energy ComplexEigenvalue{..} =
  let gamma = decayWidth
  in gamma / (2 * pi * ((energy - realPart)^2 + (gamma/2)^2))

-- | Time delay in scattering
timeDelay :: InformationOperator -> Double -> Double
timeDelay op energy =
  -- Wigner time delay: τ = ℏ dδ/dE
  let k = sqrt (energy / speedOfLight^2)
      dk_dE = 1 / (2 * speedOfLight^2 * k)
      dDelta_dk = numericalDerivative (\k' -> phaseShift op (speedOfLight^2 * k'^2) 0) k
  in dDelta_dk * dk_dE

-- | Numerical derivative helper
numericalDerivative :: (Double -> Double) -> Double -> Double
numericalDerivative f x =
  let h = 1e-6
  in (f (x + h) - f (x - h)) / (2 * h)

-- | Levinson's theorem check
levinsonTheorem :: SpectralDecomposition -> Int -> Double -> Bool
levinsonTheorem SpectralDecomposition{..} l energy =
  let nBound = length [e | RealEigenvalue e <- discreteEigenvalues, e < energy]
      k = sqrt (energy / speedOfLight^2)
      delta_0 = 0  -- Phase shift at k=0 (needs proper calculation)
      delta_inf = fromIntegral nBound * pi  -- Expected from Levinson's theorem
  in abs (delta_0 - delta_inf - fromIntegral nBound * pi) < 0.1

-- | Unitarity check for S-matrix
checkUnitarity :: InformationOperator -> Double -> [Int] -> Bool
checkUnitarity op energy lMax =
  let sElements = [sMatrixElement op energy l | l <- [0..lMax]]
      sumRule = sum [magnitude s^2 | s <- sElements]
  in abs (sumRule - fromIntegral (length sElements)) < 1e-6

-- | Friedel sum rule
friedelSum :: SpectralDecomposition -> Double -> Double
friedelSum spectral@SpectralDecomposition{..} energy =
  -- N = (2/π) Σ_l (2l+1) δ_l(E)
  let lMax = 10  -- Cutoff for sum
      phaseShifts = [0]  -- Placeholder - would calculate actual phase shifts
  in (2/pi) * sum [(2*fromIntegral l + 1) * delta | (l, delta) <- zip [0..lMax] phaseShifts]

-- | Green's function for scattering
greensFunction :: InformationOperator -> Complex Double -> Matrix (Complex Double)
greensFunction InformationOperator{..} energy =
  inv (energy * ident (rows operatorMatrix) - operatorMatrix)

-- | T-matrix for scattering
tMatrix :: InformationOperator -> InformationOperator -> Complex Double -> Matrix (Complex Double)
tMatrix freeOp fullOp energy =
  let v = operatorMatrix fullOp - operatorMatrix freeOp  -- Interaction
      g0 = greensFunction freeOp energy
  in v + v <> g0 <> tMatrix freeOp fullOp energy  -- Lippmann-Schwinger equation

-- | Optical theorem verification
verifyOpticalTheorem :: ScatteringAmplitude -> Bool
verifyOpticalTheorem scatt@ScatteringAmplitude{..} =
  let totalCS = totalCrossSection scatt
      forwardAmp = amplitude  -- Assuming this is forward scattering
      opticalCS = (4 * pi / momentum) * imagPart forwardAmp
  in abs (totalCS - opticalCS) < 1e-6

-- | Scattering length (low-energy limit)
scatteringLength :: InformationOperator -> Double
scatteringLength op =
  -- a = -lim_{k→0} δ_0(k)/k
  let smallK = 1e-6
      delta0 = phaseShift op (speedOfLight^2 * smallK^2) 0
  in -delta0 / smallK

-- | Effective range expansion
effectiveRange :: InformationOperator -> (Double, Double)
effectiveRange op =
  -- k cot δ_0 = -1/a + r_0 k²/2 + ...
  let a = scatteringLength op
      -- Would need to fit to get r_0
      r0 = 0.0  -- Placeholder
  in (a, r0)

-- | Bound state from scattering data (analytic continuation)
boundStateFromScattering :: ScatteringAmplitude -> Maybe Energy
boundStateFromScattering ScatteringAmplitude{..} =
  -- Look for poles in the upper half of complex k-plane
  if imagPart amplitude > 10  -- Arbitrary threshold indicating pole
  then Just (Energy (-speedOfLight^2 * momentum^2))  -- E = -ℏ²κ²/2m
  else Nothing

-- | Multi-channel scattering matrix
data MultiChannelScattering = MultiChannelScattering
  { channels :: Int
  , sMatrix :: Matrix (Complex Double)
  , thresholds :: [Double]
  }

-- | Calculate multi-channel S-matrix
multiChannelSMatrix :: [InformationOperator] -> Double -> MultiChannelScattering
multiChannelSMatrix ops energy =
  MultiChannelScattering
    { channels = length ops
    , sMatrix = buildMatrix n n $ \(i,j) -> 
        if i == j 
        then sMatrixElement (ops !! i) energy 0
        else 0 :+ 0  -- Simplified - no channel coupling
    , thresholds = [0.1 * fromIntegral i | i <- [1..n]]
    }
  where n = length ops

-- | Regge trajectory from resonances
reggeTrajectory :: [ComplexEigenvalue] -> [(Double, Int)]
reggeTrajectory resonances =
  -- α(t) = α_0 + α' t where t is squared energy
  [(realPart res, round (realPart res / 0.5)) | res <- resonances]

-- | Form factor from scattering amplitude
formFactor :: ScatteringAmplitude -> Double -> Complex Double
formFactor ScatteringAmplitude{..} momentumTransfer =
  amplitude * exp (0 :+ (-momentumTransfer^2 / (4 * momentum^2)))