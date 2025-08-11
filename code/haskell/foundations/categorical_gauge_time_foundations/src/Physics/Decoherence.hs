{-# LANGUAGE RecordWildCards #-}

module Physics.Decoherence where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map
import Physics.QuantumMechanics
import Physics.PageWootters

data DecoherenceProcess = DecoherenceProcess
    { dpSystemDimension :: Int
    , dpEnvironmentDimension :: Int
    , dpInteractionHamiltonian :: Matrix (Complex Double)
    , dpDecoherenceTime :: Double
    , dpPreferredBasis :: [Vector (Complex Double)]
    }

data DecoherenceFunctional = DecoherenceFunctional
    { dfHistories :: [[Vector (Complex Double)]]
    , dfAmplitudes :: [Complex Double]
    , dfConsistency :: Matrix (Complex Double)
    }

constructDecoherenceFunctional :: [[Vector (Complex Double)]] -> DecoherenceFunctional
constructDecoherenceFunctional histories = DecoherenceFunctional
    { dfHistories = histories
    , dfAmplitudes = map historyAmplitude histories
    , dfConsistency = buildConsistencyMatrix histories
    }

historyAmplitude :: [Vector (Complex Double)] -> Complex Double
historyAmplitude states = product [overlap s1 s2 | (s1, s2) <- zip states (tail states)]
  where
    overlap v1 v2 = (conj v1) <.> v2

buildConsistencyMatrix :: [[Vector (Complex Double)]] -> Matrix (Complex Double)
buildConsistencyMatrix histories =
    let n = length histories
        elements = [[decoherenceFunctionalElement h1 h2 | h2 <- histories] | h1 <- histories]
    in (n><n) $ concat elements

decoherenceFunctionalElement :: [Vector (Complex Double)] -> [Vector (Complex Double)] -> Complex Double
decoherenceFunctionalElement history1 history2 =
    if length history1 == length history2
    then product [overlap s1 s2 | (s1, s2) <- zip history1 history2]
    else 0
  where
    overlap v1 v2 = (conj v1) <.> v2

consistentHistories :: DecoherenceFunctional -> Bool
consistentHistories DecoherenceFunctional{..} =
    let offDiagonal = [dfConsistency `atIndex` (i, j) | 
                       i <- [0..rows dfConsistency - 1],
                       j <- [0..cols dfConsistency - 1],
                       i /= j]
    in all (\c -> magnitude c < 1e-10) offDiagonal

data EnvironmentModel = EnvironmentModel
    { emBathModes :: Int
    , emCouplingConstants :: [Double]
    , emTemperature :: Double
    , emSpectralDensity :: Double -> Double
    }

caldeirLeggettModel :: Int -> Double -> Double -> EnvironmentModel
caldeirLeggettModel modes coupling temp = EnvironmentModel
    { emBathModes = modes
    , emCouplingConstants = replicate modes coupling
    , emTemperature = temp
    , emSpectralDensity = ohmicSpectralDensity coupling
    }

ohmicSpectralDensity :: Double -> Double -> Double
ohmicSpectralDensity eta omega = eta * omega * exp (-omega / omega_c)
  where
    omega_c = 1.0

data MasterEquation = MasterEquation
    { meSystemHamiltonian :: Matrix (Complex Double)
    , meDissipator :: Matrix (Complex Double) -> Matrix (Complex Double)
    , meTimeEvolution :: Double -> Matrix (Complex Double) -> Matrix (Complex Double)
    }

lindbladMasterEquation :: Matrix (Complex Double) -> [Matrix (Complex Double)] -> [Double] -> MasterEquation
lindbladMasterEquation hamiltonian jumpOps rates = MasterEquation
    { meSystemHamiltonian = hamiltonian
    , meDissipator = lindbladDissipator jumpOps rates
    , meTimeEvolution = \t rho -> expm (scale t (liouvillian hamiltonian jumpOps rates)) * rho
    }

lindbladDissipator :: [Matrix (Complex Double)] -> [Double] -> Matrix (Complex Double) -> Matrix (Complex Double)
lindbladDissipator jumpOps rates rho =
    sum [rate * singleDissipator op rho | (op, rate) <- zip jumpOps rates]
  where
    singleDissipator l rho = 
        l <> rho <> (tr l) - 0.5 * ((tr l) <> l <> rho + rho <> (tr l) <> l)

liouvillian :: Matrix (Complex Double) -> [Matrix (Complex Double)] -> [Double] -> Matrix (Complex Double)
liouvillian hamiltonian jumpOps rates =
    let dim = rows hamiltonian
        superH = kronecker hamiltonian (ident dim) - kronecker (ident dim) (tr hamiltonian)
        superL = sum [rate * jumpSuperoperator op dim | (op, rate) <- zip jumpOps rates]
    in scale (0 :+ (-1)) superH + superL
  where
    jumpSuperoperator l dim =
        kronecker l (tr l) - 0.5 * (kronecker (ident dim) ((tr l) <> l) + 
                                    kronecker ((tr l) <> l) (ident dim))

decoherenceRate :: DecoherenceProcess -> Double -> Double
decoherenceRate DecoherenceProcess{..} separation =
    let coupling = 1.0
        temp = 300.0
    in coupling^2 * separation^2 * temp / dpDecoherenceTime

data QuantumDarwinism = QuantumDarwinism
    { qdSystemState :: Vector (Complex Double)
    , qdEnvironmentFragments :: [Vector (Complex Double)]
    , qdRedundancy :: Int
    , qdAccessibleInformation :: Double
    }

redundantEncoding :: Vector (Complex Double) -> Int -> QuantumDarwinism
redundantEncoding systemState numFragments = QuantumDarwinism
    { qdSystemState = systemState
    , qdEnvironmentFragments = replicate numFragments systemState
    , qdRedundancy = numFragments
    , qdAccessibleInformation = computeMutualInfo systemState systemState
    }
  where
    computeMutualInfo s1 s2 = 
        let rho1 = s1 `outer` s1
            rho2 = s2 `outer` s2
        in vonNeumannEntropy rho1 + vonNeumannEntropy rho2

data DecoherenceTimescale = DecoherenceTimescale
    { dtRelaxation :: Double
    , dtDephasing :: Double
    , dtThermalization :: Double
    , dtRecurrence :: Maybe Double
    }

computeTimescales :: DecoherenceProcess -> DecoherenceTimescale
computeTimescales DecoherenceProcess{..} = DecoherenceTimescale
    { dtRelaxation = dpDecoherenceTime
    , dtDephasing = dpDecoherenceTime / 2
    , dtThermalization = dpDecoherenceTime * fromIntegral dpEnvironmentDimension
    , dtRecurrence = if dpEnvironmentDimension < 100 
                     then Just (dpDecoherenceTime * fromIntegral (dpEnvironmentDimension^2))
                     else Nothing
    }