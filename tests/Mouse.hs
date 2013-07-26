module Mouse where

import FRP.Helm.Mouse
import Test.HUnit hiding (Test)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Utilities as Util

{- Helm's mouse variants map directly to SDL's. If that doesn't work, then it will break completely.
   So we check it here. -}
tests :: [Test]
tests = [testCase "left mouse <=> SDL" (Util.fromEnum SDL.ButtonLeft @=? fromIntegral (fromEnum LeftMouse)),
         testCase "middle mouse <=> SDL" (Util.fromEnum SDL.ButtonMiddle @=? fromIntegral (fromEnum MiddleMouse)),
         testCase "right mouse <=> SDL" (Util.fromEnum SDL.ButtonRight @=? fromIntegral (fromEnum RightMouse))]
