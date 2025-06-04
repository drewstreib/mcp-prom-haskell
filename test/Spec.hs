import Test.Hspec

import qualified MCP.TypesSpec
import qualified Prometheus.ClientSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
  MCP.TypesSpec.spec
  Prometheus.ClientSpec.spec
  IntegrationSpec.spec