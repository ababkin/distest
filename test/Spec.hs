import           Protolude
import           Test.TestTypes
import qualified Test.Unit.Network as Net


main :: IO ()
main = runSpecs defaultSpecs
  { name = "All"
  , unit =  [ Net.spec
            ]
  }


