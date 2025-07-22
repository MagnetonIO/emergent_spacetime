import Test.Hspec
import SemanticPhysics.CoreSpec
import SemanticPhysics.ConstraintsSpec
import SemanticPhysics.EmergentSpacetimeSpec

main :: IO ()
main = hspec $ do
    coreSpec
    constraintsSpec
    emergentSpacetimeSpec