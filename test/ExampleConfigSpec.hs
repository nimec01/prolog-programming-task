module ExampleConfigSpec where

import Prolog.Programming.Task (exampleConfig, verifyConfig)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "ExampleConfig" $ do
  it "should be valid" $ do
    result <- verifyConfig exampleConfig

    result `shouldBe` ()
