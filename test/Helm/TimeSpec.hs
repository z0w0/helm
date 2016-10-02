module Helm.TimeSpec where

import Helm.Time
import Test.Hspec

spec :: Spec
spec =
  describe "units" $ do
    it "represents 1 ms the same as (1/1000) s" $
      inSeconds millisecond `shouldBe` (1/1000)

    it "represents 1 s the same as (1/60) min" $
      inMinutes second `shouldBe` (1/60.0)

    it "represents 1 min as (1/60) hrs" $
      inHours minute `shouldBe` (1/60.0)
