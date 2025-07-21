{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module InformationOperator where

import SpectralTypes
import Numeric.LinearAlgebra hiding (Complex)
import qualified Numeric.LinearAlgebra as LA
import Data.Complex
import Control.Monad (forM_, when)

-- | Construct the information density operator matrix
-- Implements: Î ψ(x) = I(x)ψ(x) + ∇·[D(x)∇ψ(x)]
constructInformationOperator :: InformationField -> Int -> BoundaryCondition -> InformationOperator
constructInformationOperator InformationField{..} gridPoints boundaryCondition =
  InformationOperator
    { operatorMatrix = buildMatrix
    , boundaryCondition = boundaryCondition
    , threshold = calculateThreshold diffusionTensor
    }
  where
    dx = 1.0 / fromIntegral (gridPoints - 1)
    
    -- Build the operator matrix using finite differences
    buildMatrix = toComplex $ localTerm + diffusionTerm
    
    -- Local information density contribution: I(x)ψ(x)
    localTerm = diag localDensity
    
    -- Diffusion term: ∇·[D(x)∇ψ(x)] using centered differences
    diffusionTerm = buildDiffusionMatrix diffusionTensor dx gridPoints
    
    -- Convert real matrix to complex for general eigenvalue problems
    toComplex m = buildMatrix gridPoints gridPoints $ \(i,j) ->
      m `atIndex` (i,j) :+ 0

-- | Build the diffusion operator matrix
buildDiffusionMatrix :: Matrix Double -> Double -> Int -> Matrix Double
buildDiffusionMatrix diffTensor dx n = 
  buildMatrix n n $ \(i,j) ->
    if i == j then
      -- Diagonal terms: -2D(i)/dx²
      -2 * getDiffusionCoeff diffTensor i / (dx * dx)
    else if abs (i - j) == 1 then
      -- Off-diagonal terms: D(i+1/2)/dx² or D(i-1/2)/dx²
      let dCoeff = if j > i 
                   then getDiffusionCoeff diffTensor j
                   else getDiffusionCoeff diffTensor i
      in dCoeff / (dx * dx)
    else
      0

-- | Get diffusion coefficient at grid point
getDiffusionCoeff :: Matrix Double -> Int -> Double
getDiffusionCoeff diffTensor i = 
  if rows diffTensor == 1 
  then diffTensor `atIndex` (0, min i (cols diffTensor - 1))
  else diffTensor `atIndex` (i, i)

-- | Apply boundary conditions to the operator
applyBoundaryConditions :: InformationOperator -> InformationOperator
applyBoundaryConditions op@InformationOperator{..} =
  op { operatorMatrix = modifiedMatrix }
  where
    n = rows operatorMatrix
    modifiedMatrix = case boundaryCondition of
      Dirichlet -> applyDirichletBC operatorMatrix
      Neumann -> applyNeumannBC operatorMatrix
      Periodic -> applyPeriodicBC operatorMatrix
      Outgoing -> applyOutgoingBC operatorMatrix

-- | Apply Dirichlet boundary conditions (ψ = 0 at boundaries)
applyDirichletBC :: Matrix (Complex Double) -> Matrix (Complex Double)
applyDirichletBC mat = 
  let n = rows mat
  in buildMatrix n n $ \(i,j) ->
    if i == 0 || i == n-1 then
      if i == j then 1 :+ 0 else 0 :+ 0
    else
      mat `atIndex` (i,j)

-- | Apply Neumann boundary conditions (∂ψ/∂x = 0 at boundaries)
applyNeumannBC :: Matrix (Complex Double) -> Matrix (Complex Double)
applyNeumannBC mat =
  let n = rows mat
  in buildMatrix n n $ \(i,j) ->
    if i == 0 && j == 1 then
      mat `atIndex` (i,j) + mat `atIndex` (i,0)
    else if i == n-1 && j == n-2 then
      mat `atIndex` (i,j) + mat `atIndex` (i,n-1)
    else
      mat `atIndex` (i,j)

-- | Apply periodic boundary conditions
applyPeriodicBC :: Matrix (Complex Double) -> Matrix (Complex Double)
applyPeriodicBC mat =
  let n = rows mat
  in buildMatrix n n $ \(i,j) ->
    if i == 0 && j == n-1 then
      mat `atIndex` (0,1)
    else if i == n-1 && j == 0 then
      mat `atIndex` (n-1,n-2)
    else
      mat `atIndex` (i,j)

-- | Apply outgoing wave boundary conditions
applyOutgoingBC :: Matrix (Complex Double) -> Matrix (Complex Double)
applyOutgoingBC mat =
  let n = rows mat
      k = sqrt (1.0 :+ 0)  -- Approximate wavenumber
  in buildMatrix n n $ \(i,j) ->
    if i == 0 || i == n-1 then
      if i == j then
        1 :+ 0
      else if (i == 0 && j == 1) || (i == n-1 && j == n-2) then
        0 :+ (-1) * k
      else
        0 :+ 0
    else
      mat `atIndex` (i,j)

-- | Calculate the threshold separating discrete and continuous spectrum
calculateThreshold :: Matrix Double -> Double
calculateThreshold diffTensor = 
  let maxD = maxElement diffTensor
      minD = minElement diffTensor
  in if minD > 0 then maxD / minD else maxD

-- | Create a test information field with polynomial growth
createTestField :: Int -> InformationField
createTestField n = InformationField
  { fieldDimension = n
  , localDensity = fromList [1 + 0.1 * x^2 | i <- [0..n-1], let x = fromIntegral i / fromIntegral n]
  , diffusionTensor = ident n
  }

-- | Create information field with potential well
createPotentialWell :: Int -> Double -> Double -> InformationField
createPotentialWell n depth width = InformationField
  { fieldDimension = n
  , localDensity = fromList [potential i | i <- [0..n-1]]
  , diffusionTensor = scalar n 1.0
  }
  where
    center = fromIntegral n / 2
    potential i = 
      let x = fromIntegral i - center
      in if abs x < width 
         then -depth 
         else 0

-- | Create information field with barrier
createBarrier :: Int -> Double -> Double -> InformationField  
createBarrier n height width = InformationField
  { fieldDimension = n
  , localDensity = fromList [barrier i | i <- [0..n-1]]
  , diffusionTensor = scalar n 1.0
  }
  where
    center = fromIntegral n / 2
    barrier i =
      let x = fromIntegral i - center
      in if abs x < width
         then height
         else 0

-- | Create harmonic oscillator-like information field
createHarmonicField :: Int -> Double -> InformationField
createHarmonicField n frequency = InformationField
  { fieldDimension = n
  , localDensity = fromList [0.5 * frequency^2 * x^2 | i <- [0..n-1], let x = fromIntegral i / fromIntegral n - 0.5]
  , diffusionTensor = ident n
  }

-- | Evaluate operator action on a wave function
evaluateOperator :: InformationOperator -> WaveFunction -> WaveFunction
evaluateOperator InformationOperator{..} WaveFunction{..} =
  WaveFunction
    { amplitude = operatorMatrix #> amplitude
    , domain = domain
    }

-- | Calculate expectation value <ψ|Î|ψ>
expectationValue :: InformationOperator -> WaveFunction -> Complex Double
expectationValue op wf =
  let result = evaluateOperator op wf
      conj_amp = LA.conj (amplitude wf)
  in (conj_amp <.> amplitude result) / (conj_amp <.> amplitude wf)

-- | Calculate commutator [Î₁, Î₂]
commutator :: InformationOperator -> InformationOperator -> Matrix (Complex Double)
commutator op1 op2 =
  operatorMatrix op1 <> operatorMatrix op2 - operatorMatrix op2 <> operatorMatrix op1

-- | Check if operator is Hermitian
isHermitian :: InformationOperator -> Bool
isHermitian InformationOperator{..} =
  norm_Inf (operatorMatrix - tr (LA.conj operatorMatrix)) < 1e-10

-- | Calculate operator norm
operatorNorm :: InformationOperator -> Double
operatorNorm = norm_2 . operatorMatrix

-- | Create position operator
positionOperator :: Int -> Matrix (Complex Double)
positionOperator n = diag $ fromList [fromIntegral i :+ 0 | i <- [0..n-1]]

-- | Create momentum operator (discrete derivative)
momentumOperator :: Int -> Double -> Matrix (Complex Double)
momentumOperator n dx = 
  buildMatrix n n $ \(i,j) ->
    if j == i + 1 then
      0 :+ (-1/(2*dx))
    else if j == i - 1 then
      0 :+ (1/(2*dx))
    else
      0 :+ 0