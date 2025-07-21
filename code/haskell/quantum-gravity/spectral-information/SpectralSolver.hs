{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module SpectralSolver where

import SpectralTypes
import InformationOperator
import Numeric.LinearAlgebra hiding (Complex)
import qualified Numeric.LinearAlgebra as LA
import Data.Complex
import Data.List (sortBy, partition)
import Data.Ord (comparing)
import Control.Monad (forM)

-- | Solve the generalized eigenvalue problem Î ψ = ε ψ
solveSpectrum :: InformationOperator -> Int -> SpectralDecomposition
solveSpectrum op@InformationOperator{..} numEigenvalues =
  let (eigenvals, eigenvecs) = eigSH (realPart <$> operatorMatrix)
      eigenPairs = zip (toList eigenvals) (toColumns eigenvecs)
      sortedPairs = sortBy (comparing fst) eigenPairs
      
      -- Separate discrete and continuous spectrum based on threshold
      (discretePairs, continuousPairs) = partition ((<threshold) . fst) sortedPairs
      
      -- Convert to appropriate types
      discreteEvals = map (RealEigenvalue . fst) discretePairs
      discreteEfuncs = map (makeWaveFunction . snd) discretePairs
      
      -- Find resonances using complex analysis
      resonanceList = findResonances op
      
  in SpectralDecomposition
      { discreteEigenvalues = take numEigenvalues discreteEvals
      , discreteEigenfunctions = take numEigenvalues discreteEfuncs
      , continuousThreshold = threshold
      , resonances = resonanceList
      }
  where
    makeWaveFunction vec = WaveFunction
      { amplitude = fromList [v :+ 0 | v <- toList vec]
      , domain = fromList [fromIntegral i | i <- [0..size vec - 1]]
      }

-- | Find resonances in the complex plane
findResonances :: InformationOperator -> [ComplexEigenvalue]
findResonances InformationOperator{..} = 
  -- Simplified resonance finding - in practice would use more sophisticated methods
  [ ComplexEigenvalue { realPart = re, decayWidth = gamma }
  | re <- [0.1, 0.2 .. 1.0]
  , let gamma = resonanceWidth re
  , gamma > 0
  ]
  where
    resonanceWidth re = 0.01 * exp(-re)  -- Toy model for decay width

-- | Solve for scattering states in the continuous spectrum
solveScatteringStates :: InformationOperator -> Double -> [WaveFunction]
solveScatteringStates op@InformationOperator{..} energy =
  let k = sqrt (energy / speedOfLight^2)
      n = rows operatorMatrix
      -- Generate plane wave + scattered wave solutions
  in [ makeScatteringState k theta n | theta <- [0, pi/4 .. 2*pi]]
  where
    makeScatteringState k theta n = WaveFunction
      { amplitude = fromList [exp (0 :+ (k * x * cos theta)) | i <- [0..n-1], let x = fromIntegral i]
      , domain = fromList [fromIntegral i | i <- [0..n-1]]
      }

-- | Power iteration method for finding dominant eigenvalue
powerIteration :: InformationOperator -> WaveFunction -> Int -> (Eigenvalue, WaveFunction)
powerIteration op@InformationOperator{..} initialGuess maxIter = 
  iterate' initialGuess 0
  where
    iterate' wf iter
      | iter >= maxIter = (RealEigenvalue (realPart lambda), normalize' wf)
      | otherwise = 
          let newWf = evaluateOperator op wf
              lambda = expectationValue op newWf
              normalizedWf = normalize' newWf
          in iterate' normalizedWf (iter + 1)
    
    normalize' wf@WaveFunction{..} = 
      let norm = sqrt (realPart (LA.conj amplitude <.> amplitude))
      in wf { amplitude = amplitude / scalar (size amplitude) (norm :+ 0) }

-- | Inverse iteration for finding eigenvalue closest to a target
inverseIteration :: InformationOperator -> Complex Double -> WaveFunction -> Int -> (Eigenvalue, WaveFunction)
inverseIteration op@InformationOperator{..} target initialGuess maxIter =
  let shiftedOp = op { operatorMatrix = operatorMatrix - scalar (rows operatorMatrix) target }
      invOp = op { operatorMatrix = inv (operatorMatrix shiftedOp) }
  in powerIteration invOp initialGuess maxIter

-- | Lanczos algorithm for sparse matrices
lanczos :: InformationOperator -> WaveFunction -> Int -> (Vector Double, Matrix Double)
lanczos op@InformationOperator{..} startVec krylovDim = 
  let (alphas, betas, _) = buildKrylov startVec [] [] krylovDim
      tridiag = buildTridiagonal (fromList alphas) (fromList betas)
  in eigSH tridiag
  where
    buildKrylov _ as bs 0 = (as, bs, [])
    buildKrylov v as bs k =
      let w = evaluateOperator op v
          alpha = realPart (expectationValue op v)
          w' = if null bs 
               then w { amplitude = amplitude w - scalar (size (amplitude w)) (alpha :+ 0) <> amplitude v }
               else w { amplitude = amplitude w - scalar (size (amplitude w)) (alpha :+ 0) <> amplitude v 
                                              - scalar (size (amplitude w)) (last bs :+ 0) <> amplitude (last vs) }
          beta = sqrt (realPart (LA.conj (amplitude w') <.> amplitude w'))
          v' = w' { amplitude = amplitude w' / scalar (size (amplitude w')) (beta :+ 0) }
          vs = v : if null bs then [] else [last vs]
      in buildKrylov v' (as ++ [alpha]) (bs ++ [beta]) (k - 1)
    
    buildTridiagonal alphas betas =
      diag alphas + diagl 0 1 betas + diagl 0 (-1) betas

-- | Shooting method for finding bound states
shootingMethod :: InformationField -> BoundaryCondition -> Double -> Double -> Int -> [(Double, WaveFunction)]
shootingMethod field bc eMin eMax nPoints =
  [ (e, integrateSchrodinger field e bc) 
  | e <- linspace eMin eMax nPoints
  , checkBoundState (integrateSchrodinger field e bc)
  ]
  where
    linspace start end n = [start + fromIntegral i * (end - start) / fromIntegral (n-1) | i <- [0..n-1]]
    
    checkBoundState wf = 
      let vals = toList (amplitude wf)
          decay = all (\(i, v) -> magnitude v < 0.1) (zip [0..] (take 10 vals ++ drop (length vals - 10) vals))
      in decay

-- | Integrate Schrödinger-like equation for given energy
integrateSchrodinger :: InformationField -> Double -> BoundaryCondition -> WaveFunction
integrateSchrodinger InformationField{..} energy bc =
  -- Simplified integration - in practice would use Runge-Kutta or similar
  WaveFunction
    { amplitude = fromList [psi i | i <- [0..fieldDimension-1]]
    , domain = fromList [fromIntegral i | i <- [0..fieldDimension-1]]
    }
  where
    psi i = 
      let x = fromIntegral i / fromIntegral fieldDimension
          k = sqrt (abs (energy - localDensity `atIndex` i))
      in if energy > localDensity `atIndex` i
         then exp (0 :+ k * x)  -- Oscillating solution
         else exp ((-k * x) :+ 0)  -- Decaying solution

-- | Calculate density of states
densityOfStates :: SpectralDecomposition -> Double -> Double
densityOfStates SpectralDecomposition{..} energy =
  let discreteContribution = sum [delta (e - energy) | RealEigenvalue e <- discreteEigenvalues]
      continuousContribution = if energy >= continuousThreshold then continuousDOS energy else 0
  in discreteContribution + continuousContribution
  where
    delta x = exp (-(x^2) / 0.01) / sqrt (pi * 0.01)  -- Gaussian approximation to delta function
    continuousDOS e = sqrt (e - continuousThreshold) / (2 * pi)  -- Free particle DOS

-- | Calculate spectral measure
spectralMeasure :: SpectralDecomposition -> WaveFunction -> Double -> Complex Double
spectralMeasure SpectralDecomposition{..} testFunc energy =
  sum [weight ef * overlap ef | (eval, ef) <- zip discreteEigenvalues discreteEigenfunctions,
                                 let RealEigenvalue e = eval,
                                 let weight _ = delta (e - energy),
                                 let overlap ef = LA.conj (amplitude testFunc) <.> amplitude ef]
  where
    delta x = exp (-(x^2) / 0.01) / sqrt (pi * 0.01)

-- | Weyl's law check for spectral counting
weylLawCheck :: SpectralDecomposition -> Double -> Double
weylLawCheck SpectralDecomposition{..} energyBound =
  let count = fromIntegral $ length [e | RealEigenvalue e <- discreteEigenvalues, e <= energyBound]
      dimension = 1  -- Assuming 1D for simplicity
      volume = 1.0   -- Unit volume
      expected = volume * (energyBound / (2 * pi))^(dimension / 2)
  in count / expected

-- | Calculate spectral determinant
spectralDeterminant :: InformationOperator -> Complex Double -> Complex Double
spectralDeterminant InformationOperator{..} z =
  det (operatorMatrix - scalar (rows operatorMatrix) z)

-- | Find spectral gap
findSpectralGap :: SpectralDecomposition -> Maybe Double
findSpectralGap SpectralDecomposition{..} =
  case discreteEigenvalues of
    [] -> Nothing
    evals -> 
      let realEvals = [e | RealEigenvalue e <- evals]
      in if null realEvals || continuousThreshold <= maximum realEvals
         then Nothing
         else Just (continuousThreshold - maximum realEvals)

-- | Spectral clustering of eigenvalues
clusterEigenvalues :: [Eigenvalue] -> Double -> [[Eigenvalue]]
clusterEigenvalues eigenvals tolerance =
  foldr addToCluster [] (sortBy compareEigenvalues eigenvals)
  where
    compareEigenvalues (RealEigenvalue x) (RealEigenvalue y) = compare x y
    compareEigenvalues _ _ = EQ
    
    addToCluster eval [] = [[eval]]
    addToCluster eval (cluster:clusters) =
      case (eval, head cluster) of
        (RealEigenvalue e1, RealEigenvalue e2) ->
          if abs (e1 - e2) < tolerance
          then (eval:cluster):clusters
          else [eval]:cluster:clusters
        _ -> [eval]:cluster:clusters