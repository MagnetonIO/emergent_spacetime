module Main where

import Criterion.Main
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Physics.Functorial

main :: IO ()
main = defaultMain
  [ bgroup "entropy"
    [ bench "von Neumann 2x2" $ nf vonNeumannEntropy smallState
    , bench "von Neumann 10x10" $ nf vonNeumannEntropy largeState
    ]
  , bgroup "spacetime"
    [ bench "emergent metric" $ nf (emergentMetric smallGraph) position
    , bench "entanglement entropy" $ nf (entanglementEntropy smallGraph) subsystem
    ]
  , bgroup "constraint"
    [ bench "solve constraint" $ nf solveConstraint testConstraint
    ]
  ]

smallState :: InformationState
smallState = InformationState $ LA.scale 0.5 (LA.ident 2)

largeState :: InformationState
largeState = InformationState $ LA.scale 0.1 (LA.ident 10)

smallGraph :: EntanglementGraph
smallGraph = EntanglementGraph
  { vertices = S.fromList [v1, v2, v3]
  , edges = [Edge v1 v2 1.0, Edge v2 v3 0.5]
  , weights = M.fromList [((1,2), 1.0), ((2,3), 0.5)]
  }
  where
    v1 = Vertex 1 2 smallState
    v2 = Vertex 2 2 smallState
    v3 = Vertex 3 2 smallState

position :: V.Vector Double
position = V.fromList [0, 0, 0, 0]

subsystem :: S.Set Vertex
subsystem = S.fromList [Vertex 1 2 smallState]

testConstraint :: UnifiedConstraint
testConstraint = UnifiedConstraint
  { energyOperator = LA.scale (1 :+ 0) (LA.ident 4)
  , curvatureOperator = LA.ident 4
  , entanglementOperator = LA.scale (0.1 :+ 0) (LA.ident 4)
  , entropyOperator = LA.ident 4
  , cosmologicalConstant = 1e-52
  , newtonConstant = 1.0
  }