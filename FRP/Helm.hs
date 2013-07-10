module FRP.Helm (
  radians,
  degrees,
  turns,
  run,
  module FRP.Helm.Color,
  module FRP.Helm.Graphics,
) where

import Data.IORef
import Foreign.Ptr (castPtr)
import FRP.Elerea.Simple
import FRP.Helm.Color
import FRP.Helm.Graphics
import System.FilePath
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

data EngineState = EngineState {
  smp :: IO Element,
  {- FIXME: we need this mutable state (unfortunately) 
     because Cairo forces us to liftIO and can't return anything 
     in the render function, where the lazy image loading takes place.
     There may be a way to do this nicely, I'm just not experienced
     enough with Haskell to know how. -}
  cache :: IORef (Map.Map FilePath Cairo.Surface)
}

newEngineState :: IO Element -> IO EngineState
newEngineState smp = do
  cache <- newIORef Map.empty

  return EngineState { smp = smp, cache = cache }

{-| Initializes and runs the game engine. The supplied signal generator is
    constantly sampled  for an element to render until the user quits. -}
run :: SignalGen (Signal Element) -> IO ()
run gen = SDL.init [SDL.InitVideo] >> requestDimensions 800 600 >> start gen >>= newEngineState >>= run'

run' :: EngineState -> IO ()
run' state = do
  continue <- run''

  if continue then smp state >>= render state >> run' state else SDL.quit

run'' :: IO Bool
run'' = do
  event <- SDL.pollEvent

  case event of
    SDL.NoEvent -> return True
    SDL.Quit -> return False
    SDL.VideoResize w h -> requestDimensions w h >> run''
    _ -> run''

render :: EngineState -> Element -> IO ()
render state element = SDL.getVideoSurface >>= render' state element

render' :: EngineState -> Element -> SDL.Surface -> IO ()
render' state element screen = do
    pixels <- SDL.surfaceGetPixels screen

    Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatRGB24 w h (w * 4) $ \surface ->
      Cairo.renderWith surface (render'' w h state element)

    SDL.flip screen

  where
    w = SDL.surfaceGetWidth screen
    h = SDL.surfaceGetHeight screen

render'' :: Int -> Int -> EngineState -> Element -> Cairo.Render ()
render'' w h state element = do
  Cairo.setSourceRGB 0 0 0
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.fill

  renderElement state element

getSurface :: EngineState -> FilePath -> IO Cairo.Surface
getSurface (EngineState { cache }) src = do
  cached <- Cairo.liftIO (readIORef cache)

  case Map.lookup src cached of
    Just surface -> return surface
    Nothing -> do
      -- TODO: Use SDL_image to support more formats. I gave up after it was painful
      -- to convert between the two surface types safely.
      -- FIXME: Does this throw an error?
      surface <- Cairo.imageSurfaceCreateFromPNG src

      writeIORef cache (Map.insert src surface cached) >> return surface


renderElement :: EngineState -> Element -> Cairo.Render ()
renderElement state (CollageElement _ _ forms) = mapM (renderForm state) forms >> return ()
renderElement state (ImageElement (sx, sy) sw sh src _) = do
  surface <- Cairo.liftIO $ getSurface state (normalise src)

  Cairo.save
  Cairo.translate (-fromIntegral sx) (-fromIntegral sy)
  Cairo.setSourceSurface surface 0 0
  Cairo.translate (fromIntegral sx) (fromIntegral sy)
  Cairo.rectangle 0 0 (fromIntegral sw) (fromIntegral sh)
  Cairo.fill
  Cairo.restore

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

setFillStyle :: EngineState -> FillStyle -> Cairo.Render ()
setFillStyle _ (Solid (Color r g b a)) = Cairo.setSourceRGBA r g b a >> Cairo.fill
setFillStyle state (Texture src) = do
  surface <- Cairo.liftIO $ getSurface state (normalise src)

  Cairo.setSourceSurface surface 0 0 >> Cairo.getSource >>= (flip Cairo.patternSetExtend) Cairo.ExtendRepeat
  Cairo.fill

setFillStyle _ (Gradient (Linear (sx, sy) (ex, ey) points)) = do
  Cairo.withLinearPattern sx sy ex ey $ \pattern -> do
    Cairo.setSource pattern >> mapM (\(o, (Color r g b a)) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points >> Cairo.fill

setFillStyle _ (Gradient (Radial (sx, sy) sr (ex, ey) er points)) = do
  Cairo.withRadialPattern sx sy sr ex ey er $ \pattern -> do
    Cairo.setSource pattern >> mapM (\(o, (Color r g b a)) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points >> Cairo.fill

renderForm :: EngineState -> Form -> Cairo.Render ()
renderForm _ (Form { style = PathForm style p, .. }) =
  withTransform scalar theta x y $ 
      setLineStyle style >> Cairo.moveTo hx hy >> mapM (\(x_, y_) -> Cairo.lineTo x_ y_) p >> return ()

    where
      (hx, hy) = head p

renderForm state (Form { style = ShapeForm style shape, .. }) =
  withTransform scalar theta x y $ do
      Cairo.newPath >> Cairo.moveTo hx hy >> mapM (\(x_, y_) -> Cairo.lineTo x_ y_) shape >> Cairo.closePath

      case style of
        Left lineStyle -> setLineStyle lineStyle
        Right fillStyle -> setFillStyle state fillStyle

    where
      (hx, hy) = head shape

renderForm state (Form { style = ElementForm element, .. }) = withTransform scalar theta x y $ renderElement state element
renderForm state (Form { style = GroupForm m forms, .. }) = withTransform scalar theta x y $ Cairo.setMatrix m >> mapM (renderForm state) forms >> return ()
