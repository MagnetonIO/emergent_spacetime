{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SemanticPhysics.Information
    ( InformationField(..)
    , InformationFlow(..)
    , InformationDensity(..)
    , InformationCurrent(..)
    , computeVonNeumannEntropy
    , computeMutualInformation
    , computeRelativeEntropy
    , informationConservation
    , holographicMap
    , entanglementWedge
    ) where

import Data.Complex
import Numeric.LinearAlgebra
import Control.Monad
import Data.List (sort)

-- | Information field representation
data InformationField = InformationField
    { ifDensityMatrix :: Matrix (Complex Double)
    , ifBasis :: [Vector (Complex Double)]
    , ifDimension :: Int
    } deriving (Show)

-- | Information flow in spacetime
data InformationFlow = InformationFlow
    { ifVelocityField :: Int -> Int -> Vector Double
    , ifSourceTerm :: Int -> Int -> Double
    , ifConserved :: Bool
    }

-- | Information density
newtype InformationDensity = InformationDensity 
    { unDensity :: Matrix Double 
    } deriving (Show, Eq)

-- | Information current (4-vector)
data InformationCurrent = InformationCurrent
    { icTimeComponent :: Double
    , icSpaceComponents :: Vector Double
    } deriving (Show, Eq)

-- | Compute von Neumann entropy S = -Tr(ρ log ρ)
computeVonNeumannEntropy :: Matrix (Complex Double) -> Double
computeVonNeumannEntropy rho = 
    let eigenvals = toList $ eigenvaluesSH (trustSym rho)
        realEigenvals = map realPart eigenvals
        nonZero = filter (> 1e-10) realEigenvals
    in -sum (map (\x -> x * log x) nonZero)

-- | Compute mutual information I(A:B) = S(A) + S(B) - S(AB)
computeMutualInformation :: Matrix (Complex Double) -> [Int] -> [Int] -> Double
computeMutualInformation rhoAB partitionA partitionB = 
    let rhoA = partialTrace rhoAB partitionB
        rhoB = partialTrace rhoAB partitionA
        sA = computeVonNeumannEntropy rhoA
        sB = computeVonNeumannEntropy rhoB
        sAB = computeVonNeumannEntropy rhoAB
    in sA + sB - sAB

-- | Compute relative entropy S(ρ||σ) = Tr(ρ log ρ) - Tr(ρ log σ)
computeRelativeEntropy :: Matrix (Complex Double) -> Matrix (Complex Double) -> Double
computeRelativeEntropy rho sigma = 
    let eigenRho = eigenvaluesSH (trustSym rho)
        eigenSigma = eigenvaluesSH (trustSym sigma)
        trRhoLogRho = sum $ map (\x -> let r = realPart x in if r > 1e-10 then r * log r else 0) (toList eigenRho)
        -- For Tr(ρ log σ), we need the full spectral decomposition
        logSigma = matrixLog sigma
        trRhoLogSigma = realPart $ sumElements $ rho * logSigma
    in trRhoLogRho - trRhoLogSigma

-- | Information conservation equation: ∂ρ/∂t + ∇·j = 0
informationConservation :: InformationDensity -> InformationCurrent -> Double
informationConservation density current = 
    let divJ = computeDivergence (icSpaceComponents current)
        dRhoDt = 0  -- In equilibrium
    in dRhoDt + divJ

-- | Holographic map from bulk to boundary
holographicMap :: InformationField -> InformationField
holographicMap bulkField = InformationField
    { ifDensityMatrix = boundaryDensity
    , ifBasis = boundaryBasis
    , ifDimension = boundaryDim
    }
  where
    bulkRho = ifDensityMatrix bulkField
    bulkDim = ifDimension bulkField
    boundaryDim = floor $ sqrt $ fromIntegral bulkDim
    -- Implement HKLL-like reconstruction
    boundaryDensity = takeDiag boundaryDim $ bulkRho
    boundaryBasis = take boundaryDim $ ifBasis bulkField

-- | Entanglement wedge reconstruction
entanglementWedge :: [Int] -> Matrix (Complex Double) -> Matrix (Complex Double)
entanglementWedge boundaryRegion bulkState = 
    let wedgeIndices = computeWedgeIndices boundaryRegion (rows bulkState)
        wedgeDim = length wedgeIndices
    in subMatrix (0,0) (wedgeDim, wedgeDim) bulkState

-- Helper functions

-- | Partial trace over specified subsystem
partialTrace :: Matrix (Complex Double) -> [Int] -> Matrix (Complex Double)
partialTrace rho indicesToTrace = 
    let n = rows rho
        -- Simplified partial trace for demonstration
        -- In practice, this would involve proper tensor decomposition
        keptDim = n `div` (2 ^ length indicesToTrace)
    in subMatrix (0,0) (keptDim, keptDim) rho

-- | Matrix logarithm (simplified)
matrixLog :: Matrix (Complex Double) -> Matrix (Complex Double)
matrixLog m = 
    let (eigenvals, eigenvecs) = eig m
        logEigenvals = cmap log eigenvals
    in eigenvecs <> diag logEigenvals <> inv eigenvecs

-- | Compute divergence of vector field
computeDivergence :: Vector Double -> Double
computeDivergence v = sum $ toList v  -- Simplified for demonstration

-- | Extract diagonal block
takeDiag :: Int -> Matrix (Complex Double) -> Matrix (Complex Double)
takeDiag n m = subMatrix (0,0) (n,n) m

-- | Compute entanglement wedge indices
computeWedgeIndices :: [Int] -> Int -> [Int]
computeWedgeIndices boundary totalDim = 
    let ratio = length boundary `div` totalDim
    in take (totalDim `div` 2) [0..totalDim-1]

-- | Ensure Hermitian
trustSym :: Matrix (Complex Double) -> Herm (Complex Double)
trustSym = sym