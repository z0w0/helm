module FRP.Helm (
  radians,
  degrees,
  turns,
  run,
  module FRP.Helm.Color,
  module FRP.Helm.Graphics,
) where

import Data.IORef
import FRP.Elerea.Simple
import FRP.Helm.Color
import FRP.Helm.Graphics
import FRP.Helm.Internal (keyState)
import Graphics.Rendering.Cairo.Internal (imageSurfaceGetData)
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.Rendering.Cairo as Cairo

{-| Attempt to change the window dimensions (and initialize the video mode if not already).
    Will try to get a hardware accelerated window and then fallback to a software one.
    Throws an exception if the software mode can't be used as a fallback. -}
requestDimensions :: Int -> Int -> IO SDL.Surface
requestDimensions w h =	do
  mayhaps <- SDL.trySetVideoMode w h 32 [SDL.HWSurface, SDL.DoubleBuf, SDL.Resizable]

  case mayhaps of
    Just screen -> return screen
    Nothing -> SDL.setVideoMode w h 32 [SDL.SWSurface, SDL.Resizable]

-- |Converts radians into the standard angle measurement (radians).
radians :: Float -> Float
radians n = n

-- |Converts degrees into the standard angle measurement (radians).
degrees :: Float -> Float
degrees n = n * pi / 180

{-| Converts turns into the standard angle measurement (radians).
    Turns are essentially full revolutions of the unit circle. -}
turns :: Float -> Float
turns n = 2 * pi * n

{-| Initializes and runs the game engine. The supplied signal generator is
    constantly sampled  for an element to render until the user quits. -}
run :: SignalGen (Signal Element) -> IO ()
run gen = SDL.init [SDL.InitVideo] >> requestDimensions 800 600 >> start gen >>= run'

run' :: IO Element -> IO ()
run' smp = do
  continue <- run''

  if continue then smp >>= render >> run' smp else SDL.quit

run'' :: IO Bool
run'' = do
  event <- SDL.pollEvent
  keys <- readIORef keyState

  case event of
    SDL.NoEvent -> return True
    SDL.Quit -> return False
    SDL.VideoResize w h -> requestDimensions w h >> run''
    SDL.KeyDown (SDL.Keysym { symKey }) -> writeIORef keyState (Map.insert symKey True keys) >> run''
    SDL.KeyUp (SDL.Keysym { symKey }) -> writeIORef keyState (Map.insert symKey False keys) >> run''
    _ -> run''

render :: Element -> IO ()
render element = SDL.getVideoSurface >>= render' element

render' :: Element -> SDL.Surface -> IO ()
render' element screen = do
    src <- Cairo.createImageSurface format w h
    ptr <- imageSurfaceGetData src
    dest <- SDL.createRGBSurfaceFrom ptr w h 32 stride rshift gshift bshift 0

    Cairo.renderWith src (render'' w h element)
    SDL.blitSurface dest Nothing screen Nothing >> SDL.flip screen

  where
    w = SDL.surfaceGetWidth screen
    h = SDL.surfaceGetHeight screen
    format = Cairo.FormatARGB32
    stride = Cairo.formatStrideForWidth format w
    rshift = 0x00ff0000
    gshift = 0x0000ff00
    bshift = 0x000000ff

render'' :: Int -> Int -> Element -> Cairo.Render ()
render'' w h element = do
  Cairo.setSourceRGB 0 0 0
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.fill

  renderElement element

renderElement :: Element -> Cairo.Render ()
renderElement (CollageElement _ _ forms) = mapM renderForm forms >> return ()
renderElement (ImageElement _ ) = return () -- TODO

withTransform :: Double -> Double -> Double -> Double -> Cairo.Render () -> Cairo.Render ()
withTransform s t x y f = Cairo.save >> Cairo.scale s s >> Cairo.rotate t >> Cairo.translate x y >> f >> Cairo.restore

setLineCap :: LineCap -> Cairo.Render ()
setLineCap cap = 
  case cap of
    Flat -> Cairo.setLineCap Cairo.LineCapButt
    Round -> Cairo.setLineCap Cairo.LineCapRound
    Padded -> Cairo.setLineCap Cairo.LineCapSquare

setLineJoin :: LineJoin -> Cairo.Render ()
setLineJoin join =
  case join of
    Smooth -> Cairo.setLineJoin Cairo.LineJoinRound
    Sharp lim -> Cairo.setLineJoin Cairo.LineJoinMiter >> Cairo.setMiterLimit lim
    Clipped -> Cairo.setLineJoin Cairo.LineJoinBevel

setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle (LineStyle { color = Color r g b a, .. }) =
  Cairo.setSourceRGBA r g b a >> setLineCap cap >> setLineJoin join >>
  Cairo.setLineWidth width >> Cairo.setDash dashing dashOffset >> Cairo.stroke

setFillStyle :: FillStyle -> Cairo.Render ()
setFillStyle (Solid (Color r g b a)) = Cairo.setSourceRGBA r g b a >> Cairo.fill

renderForm :: Form -> Cairo.Render ()
renderForm (Form { style = PathForm style p, .. }) =
  withTransform scalar theta x y $ 
      setLineStyle style >> Cairo.moveTo hx hy >> mapM (\(x_, y_) -> Cairo.lineTo x_ y_) p >> return ()

    where
      (hx, hy) = head p

renderForm (Form { style = ShapeForm style shape, .. }) =
  withTransform scalar theta x y $ do
      Cairo.newPath >> Cairo.moveTo hx hy >> mapM (\(x_, y_) -> Cairo.lineTo x_ y_) shape >> Cairo.closePath

      case style of
        Left lineStyle -> setLineStyle lineStyle
        Right fillStyle -> setFillStyle fillStyle

    where
      (hx, hy) = head shape

renderForm (Form { style = ImageForm _ _ _ _, .. }) = return ()  -- TODO
renderForm (Form { style = ElementForm element, .. }) = withTransform scalar theta x y $ renderElement element
renderForm (Form { style = GroupForm m forms, .. }) = withTransform scalar theta x y $ Cairo.setMatrix m >> mapM renderForm forms >> return ()
