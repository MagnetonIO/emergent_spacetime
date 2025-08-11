{-# LANGUAGE RecordWildCards #-}

module Physics.QuantumMechanics where

import Numeric.LinearAlgebra
import Data.Complex
import qualified Data.Map as Map

type QState = Vector (Complex Double)
type QOperator = Matrix (Complex Double)
type DensityOp = Matrix (Complex Double)

data QuantumSystem = QuantumSystem
    { qsDimension :: Int
    , qsHamiltonian :: QOperator
    , qsState :: QState
    , qsObservables :: Map.Map String QOperator
    }

createQuantumSystem :: Int -> QOperator -> QState -> QuantumSystem
createQuantumSystem dim ham state = QuantumSystem
    { qsDimension = dim
    , qsHamiltonian = ham
    , qsState = state
    , qsObservables = Map.empty
    }

timeEvolution :: Double -> QuantumSystem -> QuantumSystem
timeEvolution t sys@QuantumSystem{..} =
    let evolutionOp = expm (scale (-i * t :+ 0) qsHamiltonian)
        newState = evolutionOp #> qsState
    in sys { qsState = newState }
  where
    i = 0 :+ 1

expectationValue :: QOperator -> QState -> Complex Double
expectationValue op state = (conj state) <.> (op #> state)

variance :: QOperator -> QState -> Double
variance op state =
    let expect = expectationValue op state
        expect2 = expectationValue (op <> op) state
    in realPart (expect2 - expect * expect)

commutator :: QOperator -> QOperator -> QOperator
commutator a b = a <> b - b <> a

antiCommutator :: QOperator -> QOperator -> QOperator
antiCommutator a b = a <> b + b <> a

uncertaintyRelation :: QOperator -> QOperator -> QState -> Double
uncertaintyRelation a b state =
    let varA = variance a state
        varB = variance b state
        comm = commutator a b
        expectComm = abs $ expectationValue comm state
    in sqrt (varA * varB) - expectComm / 2

data EntangledSystem = EntangledSystem
    { esSystemA :: QuantumSystem
    , esSystemB :: QuantumSystem
    , esJointState :: QState
    , esSchmidtDecomp :: [(Double, QState, QState)]
    }

createEntangledPair :: QuantumSystem -> QuantumSystem -> QState -> EntangledSystem
createEntangledPair sysA sysB joint = EntangledSystem
    { esSystemA = sysA
    , esSystemB = sysB
    , esJointState = joint
    , esSchmidtDecomp = schmidtDecomposition joint (qsDimension sysA) (qsDimension sysB)
    }

schmidtDecomposition :: QState -> Int -> Int -> [(Double, QState, QState)]
schmidtDecomposition state dimA dimB =
    let matrix = reshape dimB state
        (u, s, v) = svd matrix
        coeffs = toList s
    in [(coeff, u ! i, v ! i) | (coeff, i) <- zip coeffs [0..]]
  where
    (!) m i = flatten $ m ?? (All, Pos i)

entanglementEntropy :: EntangledSystem -> Double
entanglementEntropy EntangledSystem{..} =
    let coeffs = [c | (c, _, _) <- esSchmidtDecomp]
        probs = [c^2 | c <- coeffs, c > 1e-10]
    in -sum [p * log p | p <- probs]

reducedDensityMatrix :: EntangledSystem -> Int -> DensityOp
reducedDensityMatrix EntangledSystem{..} subsystem =
    let fullDensity = esJointState `outer` esJointState
    in if subsystem == 1
       then partialTraceB fullDensity (qsDimension esSystemA) (qsDimension esSystemB)
       else partialTraceA fullDensity (qsDimension esSystemA) (qsDimension esSystemB)

partialTraceA :: DensityOp -> Int -> Int -> DensityOp
partialTraceA density dimA dimB =
    let indices = [(i, j) | i <- [0..dimB-1], j <- [0..dimB-1]]
    in build (dimB, dimB) $ \i j ->
        sum [density `atIndex` (k*dimB + round i, k*dimB + round j) | k <- [0..dimA-1]]

partialTraceB :: DensityOp -> Int -> Int -> DensityOp
partialTraceB density dimA dimB =
    let indices = [(i, j) | i <- [0..dimA-1], j <- [0..dimA-1]]
    in build (dimA, dimA) $ \i j ->
        sum [density `atIndex` (round i*dimB + k, round j*dimB + k) | k <- [0..dimB-1]]

vonNeumannEntropy :: DensityOp -> Double
vonNeumannEntropy density =
    let eigenvals = toList $ eigenvaluesSH density
        probs = [realPart e | e <- eigenvals, realPart e > 1e-10]
    in -sum [p * log p | p <- probs]

bellState :: Int -> QState
bellState 0 = fromList [1/sqrt 2 :+ 0, 0, 0, 1/sqrt 2 :+ 0]
bellState 1 = fromList [0, 1/sqrt 2 :+ 0, 1/sqrt 2 :+ 0, 0]
bellState 2 = fromList [1/sqrt 2 :+ 0, 0, 0, (-1/sqrt 2) :+ 0]
bellState 3 = fromList [0, 1/sqrt 2 :+ 0, (-1/sqrt 2) :+ 0, 0]
bellState _ = fromList [1 :+ 0, 0, 0, 0]

pauliMatrices :: Map.Map String QOperator
pauliMatrices = Map.fromList
    [ ("I", ident 2)
    , ("X", (2><2) [0, 1, 1, 0])
    , ("Y", (2><2) [0, 0 :+ (-1), 0 :+ 1, 0])
    , ("Z", (2><2) [1, 0, 0, -1])
    ]