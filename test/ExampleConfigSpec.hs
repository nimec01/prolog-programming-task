module ExampleConfigSpec where

import Data.Maybe (isJust)
import Prolog.Programming.Task (exampleConfig, verifyConfig)
import Test.Hspec (Spec, describe, it, shouldSatisfy)

spec :: Spec
spec = describe "ExampleConfig" $ do
  it "should be valid" $
    verifyConfig exampleConfig `shouldSatisfy` isJust
