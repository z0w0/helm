-- This is an translation of http://elm-lang.org/edit/examples/Intermediate/Mario.elm
-- to Haskell using Helm game engine. Credit: klrr, ljos License: Public Domain
import Prelude hiding (Either(..))
import FRP.Elerea.Simple
import FRP.Helm
import FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

data Direction = Left | Right deriving Show

data Mario =
  Mario { mX :: Double, mY :: Double,
          vX :: Double, vY :: Double,
          dir :: Direction } deriving Show

jump :: (Int, Int) -> Mario -> Mario
jump (_, y) m@(Mario mX mY vX vY dir)
  | y < 0 && mY == 0 = Mario mX mY vX 5 dir
  | otherwise = m 

gravity :: Time -> Mario -> Mario
gravity t m@(Mario mX mY vX vY dir)
  | mY < 0 = Mario mX mY vX (vY - t / 4) dir
  | otherwise = m 

physics :: Time -> Mario -> Mario
physics t (Mario mX mY vX vY dir)
  = Mario (mX + t * vX) (min 0 (mY - t * vY)) vX vY dir

walk :: (Int, Int) -> Mario -> Mario
walk (x, _) (Mario mX mY _ vY dir) =
  Mario mX mY (fromIntegral x) vY (whatDir x)
  where
    whatDir x
      | x < 0 = Left
      | x > 0 = Right
      | otherwise = dir

step :: (Time, (Int, Int)) -> Mario -> Mario
step (t, arrows) = physics t . walk arrows . gravity t . jump arrows

render :: (Int, Int) -> Mario -> Element
render (w, h) (Mario mX1 mY1 vX1 vY1 dir1) =
  collage w h [move (half w, half h) $ filled blue $ rect (fromIntegral w) (fromIntegral h), 
               move (half w, fromIntegral h - half 50) $ filled green $ rect (fromIntegral w) 50, 
               move (mX1, mY1 + fromIntegral h - 62) $ filled red $ rect 24 28] 
  where
    half n = fromIntegral $ n `div` 2

input :: SignalGen (Signal (Time, (Int, Int)))
input = lift2 (,) delta' Keyboard.arrows
  where
    delta' = lift (/15) delta

main :: IO ()
main = run defaultConfig $ lift2 render Window.dimensions $ foldp step mario input
  where
    mario = Mario 400 0 0 0 Right
