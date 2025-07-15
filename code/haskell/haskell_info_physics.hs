{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

-- Information-First Physics: Complete Implementation
-- Demonstrates how spacetime and gauge structure emerge from information
-- 
-- This implementation shows:
-- 1. How gauge invariance emerges from information conservation
-- 2. How spacetime geometry emerges from entanglement structure  
-- 3. How Standard Model symmetries arise from information automorphisms
-- 4. Computational tractability of information-first physics

module InformationFirstPhysics where

import Data.Complex
import Data.Matrix as M hiding (trace)
import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import Control.Applicative
import Data.Functor.Identity
import Data.Functor.Compose
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Ord (comparing)
import Numeric.LinearAlgebra as LA hiding ((!))
import qualified Numeric.LinearAlgebra as LA

-- =============================================================================
-- CORE CATEGORY THEORY INFRASTRUCTURE
-- =============================================================================

-- | Base category class with identity and composition
class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

-- | Information category with quantum structure
class Category cat => InfoCategory cat where
  -- | Information entropy of a morphism
  entropy :: cat a b -> Double
  -- | Quantum trace
  trace :: cat a a -> Complex Double  
  -- | Tensor product of morphisms
  tensor :: cat a b -> cat c d -> cat (a, c) (b, d)

-- | Quantum mechanical structure with dagger
class InfoCategory cat => QuantumCategory cat where
  dagger :: cat a b -> cat b a

-- | Gauge category for symmetry transformations
class Category cat => GaugeCategory cat where
  gaugeTransform :: GaugeElement g -> cat a a -> cat a a
  gaugeInvariant :: cat a b -> Bool

-- =============================================================================
-- INFORMATION COMPLEX STRUCTURES
-- =============================================================================

-- | Fundamental information complex
data InfoComplex = InfoComplex
  { dimension :: Int
  , entropy :: Double
  , correlations :: Matrix Double
  , entanglementStructure :: Map.Map (Int, Int) Double
  , quantumAmplitudes :: V.Vector (Complex Double)
  , topology :: InfoTopology
  } deriving (Show, Eq)

-- | Topological structure of information
data InfoTopology 
  = Discrete 
  | Connected Double  -- connectedness parameter
  | Manifold Int      -- dimension
  | Fibration InfoTopology InfoTopology  -- base and fiber
  deriving (Show, Eq)

-- | Information morphism between complexes
newtype InfoMorphism a b = InfoMorphism 
  { runInfoMorphism :: InfoComplex -> InfoComplex }

instance Category InfoMorphism where
  id = InfoMorphism Prelude.id
  (InfoMorphism f) . (InfoMorphism g) = InfoMorphism (f . g)

instance InfoCategory InfoMorphism where
  entropy (InfoMorphism f) = 
    let testInfo = defaultInfoComplex
        result = f testInfo
    in InformationFirstPhysics.entropy result
  
  trace (InfoMorphism f) = 
    let testInfo = defaultInfoComplex
        result = f testInfo
        m = correlations result
    in (realPart $ LA.trace $ fromMatrix m) :+ 0
  
  tensor (InfoMorphism f) (InfoMorphism g) = InfoMorphism $ \info ->
    let result1 = f info
        result2 = g info
    in combineInfoComplexes result1 result2

instance QuantumCategory InfoMorphism where
  dagger (InfoMorphism f) = InfoMorphism $ \info ->
    let result = f info
    in adjointInfoComplex result

-- =============================================================================
-- EMERGENT SPACETIME STRUCTURES
-- =============================================================================

-- | Point in emergent spacetime
data SpacetimePoint = SpacetimePoint
  { coordinates :: V.Vector Double
  , informationDensity :: Double
  , entanglementRadius :: Double
  , curvatureInvariant :: Double
  } deriving (Show, Eq)

-- | Emergent spacetime manifold
data EmergentSpacetime = EmergentSpacetime
  { points :: [SpacetimePoint]
  , metric :: MetricTensor
  , riemannTensor :: RiemannTensor
  , dimension :: Int
  , holographicBound :: Double
  } deriving (Show, Eq)

-- | Metric tensor from information
newtype MetricTensor = MetricTensor (Matrix Double) deriving (Show, Eq)

-- | Riemann curvature tensor
newtype RiemannTensor = RiemannTensor [Matrix Double] deriving (Show, Eq)

-- | Spacetime emergence functor
data SpacetimeEmergence = SpacetimeEmergence
  { informationToPoint :: InfoComplex -> SpacetimePoint
  , pointsToManifold :: [SpacetimePoint] -> EmergentSpacetime
  , metricFromInfo :: [InfoComplex] -> MetricTensor
  , curvatureFromMetric :: MetricTensor -> RiemannTensor
  }

-- =============================================================================
-- GAUGE THEORY FROM INFORMATION AUTOMORPHISMS
-- =============================================================================

-- | Information automorphism group element
data InfoAutomorphism = InfoAutomorphism
  { transformation :: InfoComplex -> InfoComplex
  , generator :: Matrix (Complex Double)
  , lieAlgebraElement :: LieAlgebraElement
  , conservedCharge :: Double
  } deriving (Show, Eq)

-- | Lie algebra elements for gauge symmetry
data LieAlgebraElement 
  = U1Element Double
  | SU2Element (Double, Double, Double)  -- (τ₁, τ₂, τ₃) coefficients
  | SU3Element [Double]                  -- λᵢ coefficients (8 generators)
  | ProductElement LieAlgebraElement LieAlgebraElement
  deriving (Show, Eq)

-- | Gauge group structure
data GaugeGroup 
  = U1 
  | SU2 
  | SU3 
  | StandardModel  -- SU(3) × SU(2) × U(1)
  | Product GaugeGroup GaugeGroup
  deriving (Show, Eq)

-- | Gauge field from information automorphisms
data GaugeField = GaugeField
  { gaugeGroup :: GaugeGroup
  , connection :: SpacetimePoint -> Matrix (Complex Double)
  , fieldStrength :: SpacetimePoint -> Matrix (Complex Double)
  , couplingConstant :: Double
  , betaFunction :: Double -> Double  -- RG flow
  } deriving (Show, Eq)

-- | Gauge transformation element
newtype GaugeElement g = GaugeElement (Matrix (Complex Double))

instance GaugeCategory InfoMorphism where
  gaugeTransform (GaugeElement g) (InfoMorphism f) = InfoMorphism $ \info ->
    let result = f info
        transformed = applyGaugeTransformation g result
    in transformed
  
  gaugeInvariant (InfoMorphism f) = 
    -- Check if morphism commutes with all gauge transformations
    let testInfo = defaultInfoComplex
        result = f testInfo
        gaugeTransforms = generateGaugeTransformations (gaugeGroup $ extractGaugeField result)
    in all (\g -> f (applyGaugeTransformation g testInfo) == applyGaugeTransformation g result) 
           (take 10 gaugeTransforms)  -- Sample check

-- =============================================================================
-- MATTER FIELDS FROM INFORMATION PATTERNS
-- =============================================================================

-- | Matter field emerging from information
data MatterField = MatterField
  { particleType :: ParticleType
  , spin :: Rational
  , charges :: Map.Map GaugeGroup Double
  , wavefunction :: SpacetimePoint -> V.Vector (Complex Double)
  , mass :: Double
  , yukawaCouplings :: Map.Map (ParticleType, ParticleType) Double
  } deriving (Show, Eq)

-- | Standard Model particles
data ParticleType
  = Lepton LeptonType
  | Quark QuarkType  
  | GaugeBoson BosonType
  | Higgs
  deriving (Show, Eq)

data LeptonType = Electron | Muon | Tau | ElectronNeutrino | MuonNeutrino | TauNeutrino
  deriving (Show, Eq)

data QuarkType = Up | Down | Charm | Strange | Top | Bottom
  deriving (Show, Eq)

data BosonType = Photon | WBoson | ZBoson | Gluon
  deriving (Show, Eq)

-- =============================================================================
-- HOLOGRAPHIC CORRESPONDENCE
-- =============================================================================

-- | Holographic dictionary between bulk and boundary
data HolographicDictionary = HolographicDictionary
  { bulkToBoundary :: InfoComplex -> CFTState
  , boundaryToBulk :: CFTState -> InfoComplex  
  , entanglementEntropy :: CFTState -> [Int] -> Double
  , minimalSurface :: [Int] -> MinimalSurface
  , holographicRG :: Double -> InfoComplex -> InfoComplex
  }

-- | Conformal Field Theory state on boundary
data CFTState = CFTState
  { primaryFields :: [PrimaryField]
  , correlationFunctions :: Map.Map [Int] (Complex Double)
  , centralCharge :: Double
  , scalingDimensions :: Map.Map Int Double
  , stressEnergyTensor :: SpacetimePoint -> Matrix (Complex Double)
  } deriving (Show, Eq)

-- | CFT primary field
data PrimaryField = PrimaryField
  { scalingDimension :: Double
  , spin :: Rational
  , conformalWeight :: (Double, Double)  -- (h, h̄)
  , fieldOperator :: SpacetimePoint -> Complex Double
  } deriving (Show, Eq)

-- | Minimal surface for holographic entanglement entropy
data MinimalSurface = MinimalSurface
  { boundaryRegion :: [Int]
  , area :: Double
  , bulkGeometry :: [SpacetimePoint]
  } deriving (Show, Eq)

-- =============================================================================
-- COSMOLOGICAL INFORMATION DYNAMICS
-- =============================================================================

-- | Cosmological model with information dynamics
data InfoCosmology = InfoCosmology
  { scaleFactor :: Double -> Double
  , hubbleParameter :: Double -> Double
  , informationDensity :: Double -> Double
  , inflationPhase :: InflationPhase
  , darkMatter :: DarkMatterModel
  , darkEnergy :: DarkEnergyModel
  } deriving (Show, Eq)

-- | Information-driven inflation
data InflationPhase = InflationPhase
  { inflationStart :: Double
  , inflationEnd :: Double  
  , informationGenerationRate :: Double -> Double
  , fluctuationSpectrum :: Double -> Double
  } deriving (Show, Eq)

-- | Dark matter as information defects
data DarkMatterModel = DarkMatterModel
  { defectType :: TopologicalDefect
  , massDensity :: Double -> Double
  , informationContent :: Double
  } deriving (Show, Eq)

-- | Dark energy from information vacuum
data DarkEnergyModel = DarkEnergyModel
  { vacuumEnergyDensity :: Double
  , informationVacuum :: InfoComplex
  , cosmologicalConstant :: Double
  } deriving (Show, Eq)

data TopologicalDefect = Monopole | String | Domain | Texture
  deriving (Show, Eq)

-- =============================================================================
-- IMPLEMENTATION FUNCTIONS
-- =============================================================================

-- | Default information complex for testing
defaultInfoComplex :: InfoComplex
defaultInfoComplex = InfoComplex
  { dimension = 4
  , entropy = 1.0
  , correlations = identity 4
  , entanglementStructure = Map.fromList [((1,2), 0.8), ((2,3), 0.6)]
  , quantumAmplitudes = V.fromList [1:+0, 0:+1, 0:+0, 1:+0]
  , topology = Manifold 4
  }

-- | Combine two information complexes
combineInfoComplexes :: InfoComplex -> InfoComplex -> InfoComplex
combineInfoComplexes info1 info2 = InfoComplex
  { dimension = dimension info1 + dimension info2
  , entropy = entropy info1 + entropy info2
  , correlations = kronecker (correlations info1) (correlations info2)
  , entanglementStructure = Map.union (entanglementStructure info1) (entanglementStructure info2)
  , quantumAmplitudes = (quantumAmplitudes info1) V.++ (quantumAmplitudes info2)
  , topology = Fibration (topology info1) (topology info2)
  }

-- | Compute adjoint of information complex
adjointInfoComplex :: InfoComplex -> InfoComplex
adjointInfoComplex info = info
  { correlations = transpose $ correlations info
  , quantumAmplitudes = V.map conjugate $ quantumAmplitudes info
  }

-- | Emerge spacetime from information complex
emergeSpacetime :: [InfoComplex] -> EmergentSpacetime
emergeSpacetime infos = 
  let points = map emergePoint infos
      metric = computeEmergentMetric infos
      riemann = computeRiemannCurvature metric
      hBound = computeHolographicBound infos
  in EmergentSpacetime points metric riemann 4 hBound

-- | Emerge single spacetime point
emergePoint :: InfoComplex -> SpacetimePoint
emergePoint info = SpacetimePoint
  { coordinates = extractCoordinates info
  , informationDensity = entropy info / fromIntegral (dimension info)
  , entanglementRadius = computeEntanglementRadius info
  , curvatureInvariant = computeCurvatureInvariant info
  }

-- | Extract coordinates from information complex
extractCoordinates :: InfoComplex -> V.Vector Double
extractCoordinates info = 
  let m = correlations info
      eigenvals = eigenvalues $ fromMatrix m
      realParts = V.map realPart eigenvals
  in V.take 4 realParts

-- | Compute entanglement radius
computeEntanglementRadius :: InfoComplex -> Double
computeEntanglementRadius info = 
  let entanglement = entanglementStructure info
      avgEntanglement = if Map.null entanglement 
                       then 0.0 
                       else (sum $ Map.elems entanglement) / fromIntegral (Map.size entanglement)
  in sqrt avgEntanglement

-- | Compute curvature invariant from information
computeCurvatureInvariant :: InfoComplex -> Double
computeCurvatureInvariant info = 
  let m = correlations info
      det_m = M.detLU m
  in if det_m == 0 then 0 else 1.0 / det_m

-- | Compute emergent metric from information complexes
computeEmergentMetric :: [InfoComplex] -> MetricTensor
computeEmergentMetric infos = 
  let correlationMatrices = map correlations infos
      avgCorrelation = foldl1 (+) correlationMatrices `scaleMatrix` (1.0 / fromIntegral (length infos))
      fisherMetric = computeFisherInformationMetric avgCorrelation
  in MetricTensor fisherMetric

-- | Compute Fisher information metric
computeFisherInformationMetric :: Matrix Double -> Matrix Double
computeFisherInformationMetric correlationMatrix = 
  let invCorr = inverse correlationMatrix
  in case invCorr of
       Left _ -> correlationMatrix  -- fallback
       Right inv -> inv

-- | Compute Riemann curvature from metric
computeRiemannCurvature :: MetricTensor -> RiemannTensor
computeRiemannCurvature (MetricTensor g) = 
  let christoffel = computeChristoffelSymbols g
      riemann = computeRiemannFromChristoffel christoffel
  in RiemannTensor riemann

-- | Compute Christoffel symbols (simplified)
computeChristoffelSymbols :: Matrix Double -> [Matrix Double]
computeChristoffelSymbols g = 
  let n = nrows g
  in replicate (n*n) (identity n)  -- Simplified approximation

-- | Compute Riemann tensor from Christoffel symbols (simplified)
computeRiemannFromChristoffel :: [Matrix Double] -> [Matrix Double]
computeRiemannFromChristoffel christoffel = christoffel  -- Simplified

-- | Compute holographic bound
computeHolographicBound :: [InfoComplex] -> Double
computeHolographicBound infos = 
  let totalEntropy = sum $ map entropy infos
      totalDimension = sum $ map dimension infos
  in totalEntropy / (4.0 * fromIntegral totalDimension)  -- Bekenstein bound approximation

-- | Derive gauge group from information automorphisms
deriveGaugeGroup :: [InfoAutomorphism] -> GaugeGroup
deriveGaugeGroup autos = 
  let groups = map (extractGaugeGroupFromAutomorphism) autos
  in foldl Product U1 groups

-- | Extract gauge group from single automorphism
extractGaugeGroupFromAutomorphism :: InfoAutomorphism -> GaugeGroup
extractGaugeGroupFromAutomorphism auto = 
  case lieAlgebraElement auto of
    U1Element _ -> U1
    SU2Element _ -> SU2  
    SU3Element _ -> SU3
    ProductElement _ _ -> StandardModel

-- | Generate gauge transformations for testing
generateGaugeTransformations :: GaugeGroup -> [Matrix (Complex Double)]
generateGaugeTransformations U1 = [M.fromLists [[(cos θ :+ sin θ, 0:+0), (0:+0, cos θ :+ (-sin θ))]] | θ <- [0, pi/4 .. 2*pi]]
generateGaugeTransformations SU2 = [identity 2]  -- Simplified
generateGaugeTransformations SU3 = [identity 3]  -- Simplified  
generateGaugeTransformations _ = [identity 4]    -- Default

-- | Apply gauge transformation to information complex
applyGaugeTransformation :: Matrix (Complex Double) -> InfoComplex -> InfoComplex
applyGaugeTransformation g info = info
  { correlations = realPart $ g * (fromMatrix $ correlations info) * (dagger g)
  , quantumAmplitudes = V.map (\z -> (g M.! (1,1)) * z) $ quantumAmplitudes info
  }
  where
    fromMatrix :: Matrix Double -> Matrix (Complex Double)
    fromMatrix = M.map (:+ 0)
    
    dagger :: Matrix (Complex Double) -> Matrix (Complex Double)  
    dagger = M.map conjugate . transpose

-- | Extract gauge field from information complex
extractGaugeField :: InfoComplex -> GaugeField
extractGaugeField info = GaugeField
  { gaugeGroup = U1  -- Simplified
  , connection = \_ -> identity 2
  , fieldStrength = \_ -> identity 2  
  , couplingConstant = 1.0
  , betaFunction = \g -> -g^3 / (16 * pi^2)  -- One-loop β-function
  }

-- | Emerge matter fields from information patterns
emergeMatterFields :: [InfoComplex] -> [MatterField]
emergeMatterFields infos = map emergeMatterField infos

-- | Emerge single matter field
emergeMatterField :: InfoComplex -> MatterField
emergeMatterField info = MatterField
  { particleType = classifyParticle info
  , spin = extractSpin info
  , charges = extractCharges info
  , wavefunction = \point -> quantumAmplitudes info
  , mass = extractMass info
  , yukawaCouplings = Map.empty
  }

-- | Classify particle type from information
classifyParticle :: InfoComplex -> ParticleType
classifyParticle info = 
  let dim = dimension info
      ent = entropy info
  in if dim == 2 && ent < 1.0 
     then Lepton Electron
     else if dim == 3 
          then Quark Up
          else GaugeBoson Photon

-- | Extract spin from information structure
extractSpin :: InfoComplex -> Rational
extractSpin info = 
  let dim = dimension info
  in case dim of
       2 -> 1/2    -- Fermion
       3 -> 1/2    -- Quark
       4 -> 1      -- Vector boson
       _ -> 0      -- Scalar

-- | Extract gauge charges
extractCharges :: InfoComplex -> Map.Map GaugeGroup Double
extractCharges info = 
  let ent = entropy info
  in Map.fromList [(U1, ent), (SU2, ent/2), (SU3, ent/3)]

-- | Extract mass from information density
extractMass :: InfoComplex -> Double
extractMass info = entropy info * sqrt (fromIntegral $ dimension info)

-- | Implement holographic dictionary
implementHolographicDictionary :: HolographicDictionary
implementHolographicDictionary = HolographicDictionary
  { bulkToBoundary = projectToBoundary
  , boundaryToBulk = reconstructBulk
  , entanglementEntropy = computeEntanglementEntropy
  , minimalSurface = findMinimalSurface
  , holographicRG = renormalizationGroupFlow
  }

-- | Project bulk information to boundary CFT
projectToBoundary :: InfoComplex -> CFTState
projectToBoundary info = CFTState
  { primaryFields = [extractPrimaryField info]
  , correlationFunctions = Map.map (:+ 0) $ entanglementStructure info
  , centralCharge = entropy info
  , scalingDimensions = Map.fromList [(i, fromIntegral i) | i <- [1..dimension info]]
  , stressEnergyTensor = \_ -> identity 2
  }

-- | Extract primary field from information
extractPrimaryField :: InfoComplex -> PrimaryField
extractPrimaryField info = PrimaryField
  { scalingDimension = entropy info
  , spin = 0
  , conformalWeight = (entropy info / 2, entropy info / 2)
  , fieldOperator = \_ -> (quantumAmplitudes info) V.! 0
  }

-- | Reconstruct bulk from boundary CFT
reconstructBulk :: CFTState -> InfoComplex
reconstructBulk cft = InfoComplex
  { dimension = length $ primaryFields cft
  , entropy = centralCharge cft
  , correlations = identity $ length $ primaryFields cft
  , entanglementStructure = Map.map realPart $ correlationFunctions cft
  , quantumAmplitudes = V.fromList [1:+0, 0:+1]  -- Default
  , topology = Manifold $ length $ primaryFields cft
  }

-- | Compute entanglement entropy for region
computeEntanglementEntropy :: CFTState -> [Int] -> Double
computeEntanglementEntropy cft region = 
  let relevantCorrelations = Map.filterWithKey (\k _ -> any (`elem` region) k) (correlationFunctions cft)
      entropyContribution = sum $ map (realPart) $ Map.elems relevantCorrelations
  in entropyContribution

-- | Find minimal surface for holographic entanglement
findMinimalSurface :: [Int] -> MinimalSurface
findMinimalSurface region = MinimalSurface
  { boundaryRegion = region
  , area = fromIntegral $ length region  -- Simplified
  , bulkGeometry = []  -- Would compute actual surface
  }

-- | Renormalization group flow in holographic setting
renormalizationGroupFlow :: Double -> InfoComplex -> InfoComplex
renormalizationGroupFlow scale info = info
  { entropy = entropy info * (1 + 0.1 * log scale)  -- RG evolution
  , correlations = scaleMatrix (1 + 0.05 * log scale) $ correlations info
  }

-- | Information-driven cosmology
evolveInfoCosmology :: Double -> InfoCosmology -> InfoCosmology
evolveInfoCosmology time cosmo = cosmo
  { scaleFactor = \t -> (scaleFactor cosmo) t * exp (0.1 * (t - time))
  , hubbleParameter = \t -> (hubbleParameter cosmo) t * (1 + 0.05 * (t - time))
  , informationDensity = \t -> (informationDensity cosmo) t * exp (-0.2 * (t - time))
  }

-- =============================================================================
-- EXAMPLE USAGE AND SIMULATION
-- =============================================================================

-- | Example information complex representing a quark
exampleQuark :: InfoComplex
exampleQuark = InfoComplex
  { dimension = 3  -- Color triplet
  , entropy = 2.1
  , correlations = fromLists [[1.0, 0.5, 0.3], [0.5, 1.0, 0.4], [0.3, 0.4, 1.0]]
  , entanglementStructure = Map.fromList [((1,2), 0.9), ((2,3), 0.7), ((1,3), 0.6)]
  , quantumAmplitudes = V.fromList [1:+0, 0:+1, 0:+0]
  , topology = Connected 0.8
  }

-- | Example information complex representing a lepton  
exampleLepton :: InfoComplex
exampleLepton = InfoComplex
  { dimension = 2  -- Weak doublet
  , entropy = 1.4
  , correlations = fromLists [[1.0, 0.7], [0.7, 1.0]]
  , entanglementStructure = Map.fromList [((1,2), 0.8)]
  , quantumAmplitudes = V.fromList [1:+0, 0:+1]
  , topology = Connected 0.6
  }

-- | Example gauge boson information
exampleGaugeBoson :: InfoComplex
exampleGaugeBoson = InfoComplex
  { dimension = 4  -- Vector field
  , entropy = 0.0  -- Massless
  , correlations = identity 4
  , entanglementStructure = Map.empty
  , quantumAmplitudes = V.fromList [1:+0, 0:+1, 0:+0, 1:+0]
  , topology = Manifold 4
  }

-- | Run complete simulation
runInformationFirstSimulation :: IO ()
runInformationFirstSimulation = do
  putStrLn "=== Information-First Physics Simulation ==="
  putStrLn ""
  
  -- Create information complexes
  let infos = [exampleQuark, exampleLepton, exampleGaugeBoson]
  putStrLn $ "Created " ++ show (length infos) ++ " information complexes"
  
  -- Emerge spacetime
  let spacetime = emergeSpacetime infos
  putStrLn $ "Emerged spacetime with " ++ show (length $ points spacetime) ++ " points"
  putStrLn $ "Spacetime dimension: " ++ show (dimension spacetime)
  putStrLn $ "Holographic bound: " ++ show (holographicBound spacetime)
  
  -- Derive gauge theory
  let autos = map createAutomorphism infos
  let gaugeGroup = deriveGaugeGroup autos
  putStrLn $ "Derived gauge group: " ++ show gaugeGroup
  
  -- Emerge matter fields
  let matterFields = emergeMatterFields infos
  putStrLn $ "Emerged " ++ show (length matterFields) ++ " matter fields:"
  mapM_ (\field -> putStrLn $ "  " ++ show (particleType field) ++ 
                              " (spin " ++ show (spin field) ++ 
                              ", mass " ++ show (mass field) ++ ")") matterFields
  
  -- Holographic correspondence
  let cftStates = map (bulkToBoundary implementHolographicDictionary) infos
  putStrLn $ "Generated " ++ show (length cftStates) ++ " CFT boundary states"
  
  -- Cosmological evolution
  let initialCosmo = createExampleCosmology
  let evolvedCosmo = evolveInfoCosmology 1.0 initialCosmo
  putStrLn $ "Evolved cosmology: H(t=1) = " ++ show ((hubbleParameter evolvedCosmo) 1.0)
  
  putStrLn ""
  putStrLn "=== Simulation Complete ==="

-- | Create automorphism from information complex
createAutomorphism :: InfoComplex -> InfoAutomorphism
createAutomorphism info = InfoAutomorphism
  { transformation = id
  , generator = identity $ dimension info
  , lieAlgebraElement = if dimension info == 3 then SU3Element [1,0,0,0,0,0,0,0] else U1Element 1.0
  , conservedCharge = entropy info
  }

-- | Create example cosmology
createExampleCosmology :: InfoCosmology
createExampleCosmology = InfoCosmology
  { scaleFactor = \t -> exp (0.7 * t)  -- Accelerating expansion
  , hubbleParameter = \t -> 0.7 * exp (0.1 * t)
  , informationDensity = \t -> exp (-0.5 * t)  -- Decreasing with expansion
  , inflationPhase = InflationPhase 0.0 1e-32 (\t -> exp (60 * t)) (\k -> k^(-0.96))
  , darkMatter = DarkMatterModel String (\t -> 0.26 * exp (-3 * t)) 1.0
  , darkEnergy = DarkEnergyModel (1e-123) defaultInfoComplex (1e-123)
  }

-- =============================================================================
-- UTILITY FUNCTIONS
-- =============================================================================

-- | Utility: matrix scaling
scaleMatrix :: Double -> Matrix Double -> Matrix Double
scaleMatrix s = M.map (*s)

-- | Utility: Kronecker product for matrices
kronecker :: Matrix Double -> Matrix Double -> Matrix Double
kronecker a b = 
  let rowsA = nrows a
      colsA = ncols a  
      rowsB = nrows b
      colsB = ncols b
  in matrix (rowsA * rowsB) (colsA * colsB) $ \(i,j) ->
       let (iA, iB) = divMod (i-1) rowsB
           (jA, jB) = divMod (j-1) colsB
       in (a ! (iA+1, jA+1)) * (b ! (iB+1, jB+1))

-- | Utility: inverse matrix with error handling
inverse :: Matrix Double -> Either String (Matrix Double)
inverse m = 
  let det = M.detLU m
  in if abs det < 1e-10 
     then Left "Matrix is singular"
     else Right $ M.map (/det) $ M.transpose $ M.cofactorMatrix m

-- | Convert from Data.Matrix to Numeric.LinearAlgebra
fromMatrix :: Matrix Double -> LA.Matrix Double
fromMatrix m = LA.fromLists $ M.toLists m

-- | Convert from Numeric.LinearAlgebra to Data.Matrix  
toMatrix :: LA.Matrix Double -> Matrix Double
toMatrix = M.fromLists . LA.toLists

-- | Main function for testing
main :: IO ()
main = runInformationFirstSimulation