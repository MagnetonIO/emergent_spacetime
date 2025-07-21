{-# LANGUAGE TypeOperators #-}
module InfoEnergy.Quantum where

import InfoEnergy.Core
import qualified Numeric.LinearAlgebra as LA
import Data.Complex
import Control.Monad

type Ket = LA.Vector (Complex Double)
type Bra = LA.Vector (Complex Double)
type Operator = LA.Matrix (Complex Double)

data QuantumState = Pure Ket | Mixed Operator

data QuantumInfoSpace = QInfoSpace
  { qiHilbertDim :: Int
  , qiStates :: [QuantumState]
  , qiVonNeumannEntropy :: QuantumState -> Entropy
  }

data QuantumThermoSystem = QThermoSystem
  { qtHamiltonian :: Operator
  , qtTemperature :: Temperature
  , qtGibbsState :: Operator
  }

data QuantumIESystem = QIESystem
  { qieInfo :: QuantumInfoSpace
  , qieThermo :: QuantumThermoSystem
  , qieCorrespondence :: Operator -> QuantumState
  }

newtype CPTP a b = CPTP { runCPTP :: a -> b }

instance Category CPTP where
  id = CPTP id
  CPTP g . CPTP f = CPTP (g . f)

densityMatrix :: QuantumState -> Operator
densityMatrix (Pure ket) = ket `LA.outer` LA.conj ket
densityMatrix (Mixed rho) = rho

trace :: Operator -> Complex Double
trace = LA.sumElements . LA.takeDiag

purity :: QuantumState -> Double
purity state = realPart $ trace (rho LA.<> rho)
  where rho = densityMatrix state

quantumVonNeumannEntropy :: QuantumState -> Entropy
quantumVonNeumannEntropy state = 
  let rho = densityMatrix state
      (eigenvals, _) = LA.eig rho
      probs = [ realPart e | e <- LA.toList eigenvals, realPart e > 1e-10 ]
  in Entropy $ negate $ sum [ p * logBase 2 p | p <- probs ]

quantumGibbsState :: Operator -> Temperature -> Operator
quantumGibbsState hamiltonian (Temp t) =
  let beta = 1 / (boltzmannConstant * t)
      expH = LA.expm $ LA.scale (-beta) hamiltonian
      z = trace expH
  in LA.scale (1 / z) expH

entanglementEntropy :: Int -> QuantumState -> Int -> Entropy
entanglementEntropy totalDim state subsysDim =
  let rho = densityMatrix state
      reduced = partialTrace totalDim subsysDim rho
  in quantumVonNeumannEntropy (Mixed reduced)

partialTrace :: Int -> Int -> Operator -> Operator
partialTrace totalDim subDim mat = 
  let otherDim = totalDim `div` subDim
  in LA.fromLists $ 
     [ [ sum [ mat LA.! (i + k*subDim, j + k*subDim) 
             | k <- [0..otherDim-1] ]
       | j <- [0..subDim-1] ]
     | i <- [0..subDim-1] ]

tensorProduct :: Operator -> Operator -> Operator
tensorProduct = LA.kronecker

data EntanglementMeasure = EntanglementMeasure
  { measureName :: String
  , computeMeasure :: QuantumState -> Double
  }

concurrence :: QuantumState -> QuantumState -> Double
concurrence state1 state2 = 
  let rho = densityMatrix $ tensorState state1 state2
  in 0.0
  where
    tensorState (Pure k1) (Pure k2) = Pure (k1 `tensorProd` k2)
    tensorState _ _ = error "Mixed state tensor not implemented"
    tensorProd v1 v2 = LA.flatten $ v1 `LA.outer` v2

quantumLandauer :: QuantumState -> QuantumState -> Temperature -> Energy
quantumLandauer initialState finalState temp =
  let deltaS = unEntropy (quantumVonNeumannEntropy finalState) 
             - unEntropy (quantumVonNeumannEntropy initialState)
  in landauerPrinciple (Entropy deltaS) temp

quantumYonedaFunctor :: QuantumIESystem -> (Operator -> Operator) -> QuantumState
quantumYonedaFunctor qie@(QIESystem info thermo corr) morphism =
  let rho = qtGibbsState thermo
      transformed = morphism rho
  in corr transformed