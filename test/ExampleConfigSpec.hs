module ExampleConfigSpec where

import Data.Maybe (isJust, isNothing)
import Prolog.Programming.Data (Config (..))
import Prolog.Programming.Task (exampleConfig, verifyConfig)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "ExampleConfig" $ do
  it "should be valid" $
    verifyConfig exampleConfig `shouldSatisfy` isJust
  it "rejects config with unknown fields" $
    verifyConfig (Config "globalTiimeout: 1000") `shouldSatisfy` isNothing
