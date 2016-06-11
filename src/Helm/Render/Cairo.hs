module Helm.Render.Cairo (
  -- * Rendering
  render
) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (get)

import Data.Foldable (forM_)
import qualified Data.Text as T
import Linear.V2 (V2(V2))
import Linear.V3 (V3(V3))
import Foreign.Ptr (castPtr)

import Helm.Engine (Engine(..))
import Helm.Render (Render(..))
import Helm.Color (Color(..), Gradient(..))
import Helm.Graphics2D

import Graphics.Rendering.Cairo.Matrix (Matrix(..))
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango

import qualified SDL
import qualified SDL.Video as Video
import qualified SDL.Video.Renderer as Renderer

render :: Element -> Render ()
render element = Render $ do
  Engine { window, renderer } <- get

  lift $ do
    dims@(V2 w h) <- SDL.get $ Video.windowSize window

    texture <- Renderer.createTexture renderer Renderer.ARGB8888 Renderer.TextureAccessStreaming dims
    (pixels, pitch) <- Renderer.lockTexture texture Nothing

    Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h) (fromIntegral pitch) $ \surface ->
      Cairo.renderWith surface $ do
        Cairo.setSourceRGB 0 0 0
        Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
        Cairo.fill

        renderElement element

    Renderer.unlockTexture texture
    Renderer.clear renderer
    Renderer.copy renderer texture Nothing Nothing
    Renderer.destroyTexture texture
    Renderer.present renderer

renderElement :: Element -> Cairo.Render ()
renderElement (CollageElement w h center forms) = do
  Cairo.save
  Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
  Cairo.clip

  forM_ center $ uncurry Cairo.translate
  mapM_ renderForm forms

  Cairo.restore

renderElement (ImageElement (sx, sy) sw sh src stretch) = do
  return ()

renderElement (TextElement (Text { textColor = (Color r g b a), .. })) = do
    Cairo.save

    layout <- Pango.createLayout textUTF8

    Cairo.liftIO $ Pango.layoutSetAttributes layout
      [ Pango.AttrFamily { paStart = i, paEnd = j, paFamily = T.pack textTypeface }
      , Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight }
      , Pango.AttrStyle  { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle }
      , Pango.AttrSize   { paStart = i, paEnd = j, paSize = textHeight }
      ]

    Pango.PangoRectangle x y w h <- fmap snd $ Cairo.liftIO $ Pango.layoutGetExtents layout

    Cairo.translate ((-w / 2) -x) ((-h / 2) - y)
    Cairo.setSourceRGBA r g b a
    Pango.showLayout layout
    Cairo.restore

  where
    i = 0
    j = length textUTF8

{-| A utility function that maps to a Pango font weight based off our variant. -}
mapFontWeight :: FontWeight -> Pango.Weight
mapFontWeight weight = case weight of
  LightWeight  -> Pango.WeightLight
  NormalWeight -> Pango.WeightNormal
  BoldWeight   -> Pango.WeightBold

{-| A utility function that maps to a Pango font style based off our variant. -}
mapFontStyle :: FontStyle -> Pango.FontStyle
mapFontStyle style = case style of
  NormalStyle  -> Pango.StyleNormal
  ObliqueStyle -> Pango.StyleOblique
  ItalicStyle  -> Pango.StyleItalic

{-| A utility function that goes into a state of transformation and then pops
    it when finished. -}
withTransform :: Double -> Double -> Double -> Double -> Cairo.Render () -> Cairo.Render ()
withTransform s t x y f = do
  Cairo.save
  Cairo.scale s s
  Cairo.translate x y
  Cairo.rotate t
  f
  Cairo.restore

{-| A utility function that sets the Cairo line cap based off of our version. -}
setLineCap :: LineCap -> Cairo.Render ()
setLineCap cap = case cap of
  FlatCap   -> Cairo.setLineCap Cairo.LineCapButt
  RoundCap  -> Cairo.setLineCap Cairo.LineCapRound
  PaddedCap -> Cairo.setLineCap Cairo.LineCapSquare

{-| A utility function that sets the Cairo line style based off of our version. -}
setLineJoin :: LineJoin -> Cairo.Render ()
setLineJoin join = case join of
  SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
  SharpJoin lim -> Cairo.setLineJoin Cairo.LineJoinMiter >> Cairo.setMiterLimit lim
  ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a line style and then strokes afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle (LineStyle { lineColor = Color r g b a, .. }) = do
  Cairo.setSourceRGBA r g b a
  setLineCap lineCap
  setLineJoin lineJoin
  Cairo.setLineWidth lineWidth
  Cairo.setDash lineDashing lineDashOffset
  Cairo.stroke

{-| A utility function that sets up all the necessary settings with Cairo
    to render with a fill style and then fills afterwards. Assumes
    that all drawing paths have already been setup before being called. -}
setFillStyle :: FillStyle -> Cairo.Render ()
setFillStyle (Solid (Color r g b a)) = do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

setFillStyle (Texture src) = do
  return ()

setFillStyle (Gradient (Linear (sx, sy) (ex, ey) points)) =
  Cairo.withLinearPattern sx sy ex ey $ \pattern ->
    setGradientFill pattern points

setFillStyle (Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  Cairo.withRadialPattern sx sy sr ex ey er $ \pattern ->
    setGradientFill pattern points

{-| A utility function that adds color stops to a pattern and then fills it. -}
setGradientFill :: Cairo.Pattern -> [(Double, Color)] -> Cairo.Render ()
setGradientFill pattern points = do
  Cairo.setSource pattern
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA pattern o r g b a) points
  Cairo.fill

{-| A utility that renders a form. -}
renderForm :: Form -> Cairo.Render ()
renderForm Form { .. } = withTransform formScale formTheta formX formY $
  case formStyle of
    PathForm style (Path (~ps @ ((hx, hy) : _))) -> do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (uncurry Cairo.lineTo) ps
      setLineStyle style

    ShapeForm style shape -> do
      Cairo.newPath

      case shape of
        PolygonShape (Path (~ps @ ((hx, hy) : _))) -> do
          Cairo.moveTo hx hy
          mapM_ (uncurry Cairo.lineTo) ps

        RectangleShape (w, h) ->
          Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (cx, cy) a1 a2 r (sx, sy) -> do
          Cairo.scale sx sy
          Cairo.arc cx cy r a1 a2
          Cairo.scale 1 1

      either setLineStyle setFillStyle style

    ElementForm element -> renderElement element

    GroupForm mayhaps forms -> do
      Cairo.save

      forM_ mayhaps $ \(Transform (V3 (V3 a b x) (V3 c d y) (V3 0 0 1))) ->
        Cairo.setMatrix $ Matrix a b c d x y

      mapM_ renderForm forms
      Cairo.restore
