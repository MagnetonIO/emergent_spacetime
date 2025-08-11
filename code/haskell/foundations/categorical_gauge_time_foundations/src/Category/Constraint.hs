{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Category.Constraint where

import Category.Base
import Numeric.LinearAlgebra
import Data.Complex

type WaveFunction = Vector (Complex Double)
type HamiltonianOperator = Matrix (Complex Double)

data PhysicalState = PhysicalState
    { stateVector :: WaveFunction
    , stateConstraints :: [HamiltonianOperator]
    , stateLabel :: String
    } deriving (Show)

data ConstraintPreservingMap = ConstraintPreservingMap
    { mapUnitary :: Matrix (Complex Double)
    , mapConstraints :: [HamiltonianOperator]
    } deriving (Show)

instance Category ConstraintCategory where
    type Ob ConstraintCategory = PhysicalState
    type Mor ConstraintCategory = ConstraintPreservingMap
    
    idMor = ConstraintPreservingMap (ident 1) []
    
    compose map2 map1 = ConstraintPreservingMap
        { mapUnitary = mapUnitary map2 <> mapUnitary map1
        , mapConstraints = mapConstraints map1 ++ mapConstraints map2
        }
    
    source _ = PhysicalState (fromList [1]) [] "source"
    target _ = PhysicalState (fromList [1]) [] "target"

data ConstraintCategory

checkWheelerDeWitt :: PhysicalState -> HamiltonianOperator -> Bool
checkWheelerDeWitt PhysicalState{..} hamiltonian =
    let result = hamiltonian #> stateVector
        tolerance = 1e-10
    in norm_2 result < tolerance

applyConstraint :: HamiltonianOperator -> WaveFunction -> WaveFunction
applyConstraint hamiltonian state = hamiltonian #> state

data WheelerDeWittSpace = WheelerDeWittSpace
    { wdwStates :: [PhysicalState]
    , wdwHamiltonian :: HamiltonianOperator
    , wdwInnerProduct :: WaveFunction -> WaveFunction -> Complex Double
    }

constructWDWSpace :: HamiltonianOperator -> [WaveFunction] -> WheelerDeWittSpace
constructWDWSpace hamiltonian states = WheelerDeWittSpace
    { wdwStates = [PhysicalState s [hamiltonian] ("state" ++ show i) | 
                   (s, i) <- zip states [0..]]
    , wdwHamiltonian = hamiltonian
    , wdwInnerProduct = \v1 v2 -> (conj v1) <.> v2
    }

timelessEvolution :: WheelerDeWittSpace -> Bool
timelessEvolution WheelerDeWittSpace{..} =
    all (\state -> checkWheelerDeWitt state wdwHamiltonian) wdwStates

data DiracObservable = DiracObservable
    { diracOperator :: Matrix (Complex Double)
    , diracCommutesWithConstraints :: Bool
    }

checkDiracObservable :: DiracObservable -> HamiltonianOperator -> Bool
checkDiracObservable DiracObservable{..} hamiltonian =
    let commutator = diracOperator <> hamiltonian - hamiltonian <> diracOperator
    in norm_Frob commutator < 1e-10

physicalInnerProduct :: WheelerDeWittSpace -> WaveFunction -> WaveFunction -> Complex Double
physicalInnerProduct wdw state1 state2 = wdwInnerProduct wdw state1 state2

riggingMap :: WaveFunction -> WaveFunction -> Complex Double
riggingMap bra ket = (conj bra) <.> ket