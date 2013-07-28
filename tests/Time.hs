module Time where

import FRP.Helm.Time
import Test.HUnit hiding (Test)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit

tests :: [Test]
tests = [testCase "1 ms is (1/1000) s" (inSeconds millisecond @=? 0.001),
         testCase "1 s is (1/60) mins" (inMinutes second @=? 1 / 60),
         testCase "1 min is (1/60) hours" (inHours minute @=? 1 / 60)]
