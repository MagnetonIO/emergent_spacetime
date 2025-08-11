{-# LANGUAGE RecordWildCards #-}

module Applications.QuantumGravity where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map
import qualified Data.Set as Set
import Physics.WheelerDeWitt
import Category.HigherGauge

data LoopQuantumGravity = LoopQuantumGravity
    { lqgSpinNetworks :: [SpinNetwork]
    , lqgAreaOperator :: SpinNetwork -> Double
    , lqgVolumeOperator :: SpinNetwork -> Double
    , lqgHamiltonian :: SpinNetwork -> SpinNetwork
    }

data SpinFoamModel = SpinFoamModel
    { sfmVertices :: Set.Set FoamVertex
    , sfmFaces :: Set.Set FoamFace
    , sfmAmplitude :: SpinFoam -> Complex Double
    }

data FoamVertex = FoamVertex Int deriving (Eq, Ord, Show)
data FoamFace = FoamFace Int Int deriving (Eq, Ord, Show)
data SpinFoam = SpinFoam [FoamVertex] [FoamFace]

computeSpinFoamAmplitude :: SpinFoamModel -> SpinFoam -> Complex Double
computeSpinFoamAmplitude model foam = sfmAmplitude model foam

data CausalDynamicalTriangulation = CDT
    { cdtSimplicialComplex :: SimplicialComplex
    , cdtTimeSlices :: [TimeSlice]
    , cdtAction :: SimplicialComplex -> Double
    , cdtMonteCarlo :: IO SimplicialComplex
    }

data SimplicialComplex = SimplicialComplex
    { scVertices :: Set.Set Int
    , scSimplices :: Set.Set [Int]
    , scDimension :: Int
    }

data TimeSlice = TimeSlice
    { tsTime :: Double
    , tsGeometry :: SimplicialComplex
    }

reggeAction :: SimplicialComplex -> Double
reggeAction SimplicialComplex{..} =
    let deficit = computeDeficitAngles scSimplices
    in sum [d^2 | d <- deficit]
  where
    computeDeficitAngles _ = [0.1, 0.2, 0.3]

data AsymptoticSafety = AsymptoticSafety
    { asRGFlow :: Double -> (Double, Double, Double)
    , asFixedPoint :: (Double, Double, Double)
    , asCriticalExponents :: [Double]
    , asUVCompletion :: Bool
    }

renormalizationGroupFlow :: Double -> Double -> Double -> Double -> (Double, Double, Double)
renormalizationGroupFlow g lambda newton k =
    let beta_g = 2 * g - g^2 / (1 + g)
        beta_lambda = -2 * lambda + lambda^2
        beta_newton = 2 * newton
    in (g + k * beta_g, lambda + k * beta_lambda, newton + k * beta_newton)

data StringTheoryBackground = StringTheoryBackground
    { stbMetric :: Matrix Double
    , stbDilaton :: Double -> Double
    , stbBField :: Matrix Double
    , stbRamondFluxes :: [Matrix Double]
    }

data MatrixModel = MatrixModel
    { mmMatrices :: [Matrix (Complex Double)]
    , mmAction :: [Matrix (Complex Double)] -> Double
    , mmPartitionFunction :: Complex Double
    }

bfssAction :: [Matrix (Complex Double)] -> Double
bfssAction matrices =
    let commutators = [norm_Frob (commutator m1 m2) | m1 <- matrices, m2 <- matrices, m1 /= m2]
    in sum commutators
  where
    commutator a b = a <> b - b <> a

data AdSCFTCorrespondence = AdSCFT
    { adsBulkTheory :: BulkGravity
    , cftBoundaryTheory :: ConformalFieldTheory
    , holographicDictionary :: Map.Map String String
    }

data BulkGravity = BulkGravity
    { bgMetric :: Matrix Double
    , bgCosmologicalConstant :: Double
    }

data ConformalFieldTheory = CFT
    { cftCentralCharge :: Double
    , cftOperators :: Map.Map String Double
    }

rtFormula :: AdSCFTCorrespondence -> Double -> Double
rtFormula AdSCFT{..} area = area / (4 * g_newton)
  where
    g_newton = 1.0

data EmergentSpacetime = EmergentSpacetime
    { esEntanglementStructure :: Matrix Double
    , esMetricTensor :: Matrix Double -> Matrix Double
    , esConnectivity :: Matrix Bool
    , esCausalStructure :: Matrix Bool
    }

vanRaamsdonkConjecture :: Matrix Double -> Matrix Double
vanRaamsdonkConjecture entanglement =
    let dim = rows entanglement
    in build (dim, dim) $ \i j -> 
        exp (-(entanglement `atIndex` (round i, round j)))

data BlackHoleThermodynamics = BlackHoleThermodynamics
    { bhtTemperature :: Double
    , bhtEntropy :: Double
    , bhtMass :: Double
    , bhtAngularMomentum :: Double
    }

bekensteinHawkingEntropy :: Double -> Double
bekensteinHawkingEntropy area = area / 4

hawkingTemperature :: Double -> Double
hawkingTemperature mass = 1 / (8 * pi * mass)

data InformationParadox = InformationParadox
    { ipInitialState :: Vector (Complex Double)
    , ipHorizonDynamics :: Vector (Complex Double) -> Vector (Complex Double)
    , ipPageCurve :: Double -> Double
    , ipIslandFormula :: Double -> Double
    }

pageTime :: Double -> Double
pageTime entropy = entropy / 2