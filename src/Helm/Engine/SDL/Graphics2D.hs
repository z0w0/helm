-- | Contains the SDL implementation 2D graphics rendering implementation (uses Cairo).
module Helm.Engine.SDL.Graphics2D (render) where

import           Data.Foldable (forM_)
import           Foreign.C.Types (CInt)
import           Foreign.Ptr (castPtr)

import qualified Data.Text as T
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Pango as Pango
import           Graphics.Rendering.Cairo.Matrix (Matrix(..))
import           Linear.V2 (V2(V2))
import           Linear.V3 (V3(V3))
import qualified SDL.Video.Renderer as Renderer

import           Helm.Color (Color(..), Gradient(..))
import           Helm.Engine.SDL.Asset (Image(..))
import           Helm.Engine.SDL.Engine (SDLEngine)
import           Helm.Graphics2D
import           Helm.Graphics2D.Text

-- | Renders a 2D element to an SDL texture (under a width and height).
render :: Renderer.Texture -> V2 CInt -> Collage SDLEngine -> IO ()
render tex (V2 w h) coll = do
  (pixels, pitch) <- Renderer.lockTexture tex Nothing

  Cairo.withImageSurfaceForData (castPtr pixels) Cairo.FormatARGB32 (fromIntegral w) (fromIntegral h) (fromIntegral pitch) $ \surface ->
    Cairo.renderWith surface $ do
      Cairo.setSourceRGB 0 0 0
      Cairo.rectangle 0 0 (fromIntegral w) (fromIntegral h)
      Cairo.fill

      renderCollage coll

  Renderer.unlockTexture tex

renderCollage :: Collage SDLEngine -> Cairo.Render ()
renderCollage Collage { .. } = do
  Cairo.save

  forM_ collageDims $ \(V2 w h) -> do
    Cairo.rectangle 0 0 w h
    Cairo.clip

  forM_ collageCenter $ \(V2 x y) -> Cairo.translate x y
  mapM_ renderForm collageForms

  Cairo.restore

-- | A utility function that maps to a Pango font weight based off our variant.
mapFontWeight :: FontWeight -> Pango.Weight
mapFontWeight weight = case weight of
  LightWeight  -> Pango.WeightLight
  NormalWeight -> Pango.WeightNormal
  BoldWeight   -> Pango.WeightBold

-- | A utility function that maps to a Pango font style based off our variant.
mapFontStyle :: FontStyle -> Pango.FontStyle
mapFontStyle style = case style of
  NormalStyle  -> Pango.StyleNormal
  ObliqueStyle -> Pango.StyleOblique
  ItalicStyle  -> Pango.StyleItalic

-- | A utility function that goes into a state of transformation and then pops it when finished.
withTransform :: Double -> Double -> Double -> Double -> Cairo.Render () -> Cairo.Render ()
withTransform s t x y f = do
  Cairo.save
  Cairo.scale s s
  Cairo.translate x y
  Cairo.rotate t
  f
  Cairo.restore

-- | A utility function that sets the Cairo line cap based off of our version.
setLineCap :: LineCap -> Cairo.Render ()
setLineCap cap = case cap of
  FlatCap   -> Cairo.setLineCap Cairo.LineCapButt
  RoundCap  -> Cairo.setLineCap Cairo.LineCapRound
  PaddedCap -> Cairo.setLineCap Cairo.LineCapSquare

-- | A utility function that sets the Cairo line style based off of our version.
setLineJoin :: LineJoin -> Cairo.Render ()
setLineJoin join = case join of
  SmoothJoin    -> Cairo.setLineJoin Cairo.LineJoinRound
  SharpJoin lim -> Cairo.setLineJoin Cairo.LineJoinMiter >> Cairo.setMiterLimit lim
  ClippedJoin   -> Cairo.setLineJoin Cairo.LineJoinBevel

-- | A utility function that sets up all the necessary settings with Cairo
-- to render with a line style and then strokes afterwards. Assumes
-- that all drawing paths have already been setup before being called.
setLineStyle :: LineStyle -> Cairo.Render ()
setLineStyle LineStyle { lineColor = Color r g b a, .. } = do
  Cairo.setSourceRGBA r g b a
  setLineCap lineCap
  setLineJoin lineJoin
  Cairo.setLineWidth lineWidth
  Cairo.setDash lineDashing lineDashOffset
  Cairo.stroke

-- | A utility function that sets up all the necessary settings with Cairo
-- to render with a fill style and then fills afterwards. Assumes
-- that all drawing paths have already been setup before being called.
setFillStyle :: FillStyle SDLEngine -> Cairo.Render ()
setFillStyle (Solid (Color r g b a)) = do
  Cairo.setSourceRGBA r g b a
  Cairo.fill

setFillStyle (Texture SDLImage { cairoSurface }) = do
  Cairo.setSourceSurface cairoSurface 0 0
  Cairo.getSource >>= flip Cairo.patternSetExtend Cairo.ExtendRepeat
  Cairo.fill

setFillStyle (Gradient (Linear (sx, sy) (ex, ey) points)) =
  Cairo.withLinearPattern sx sy ex ey $ \ptn ->
    setGradientFill ptn points

setFillStyle (Gradient (Radial (sx, sy) sr (ex, ey) er points)) =
  Cairo.withRadialPattern sx sy sr ex ey er $ \ptn ->
    setGradientFill ptn points

-- | A utility function that adds color stops to a pattern and then fills it.
setGradientFill :: Cairo.Pattern -> [(Double, Color)] -> Cairo.Render ()
setGradientFill ptn points = do
  Cairo.setSource ptn
  mapM_ (\(o, Color r g b a) -> Cairo.patternAddColorStopRGBA ptn o r g b a) points
  Cairo.fill

-- | A utility that renders a form.
renderForm :: Form SDLEngine -> Cairo.Render ()
renderForm Form { formPos = V2 x y, .. } = withTransform formScale formTheta x y $ do
  Cairo.save

  case formStyle of
    PathForm style (Path (~ps @ (V2 hx hy : _))) -> do
      Cairo.newPath
      Cairo.moveTo hx hy
      mapM_ (\(V2 lx ly) -> Cairo.lineTo lx ly) ps
      setLineStyle style

    ShapeForm style shape -> do
      Cairo.newPath

      case shape of
        PolygonShape (Path (~ps @ (V2 hx hy : _))) -> do
          Cairo.moveTo hx hy
          mapM_ (\(V2 lx ly) -> Cairo.lineTo lx ly) ps

        RectangleShape (V2 w h) ->
          Cairo.rectangle (-w / 2) (-h / 2) w h

        ArcShape (V2 cx cy) a1 a2 r (V2 sx sy) -> do
          Cairo.scale sx sy
          Cairo.arc cx cy r a1 a2

      case style of
        OutlinedShape ls -> setLineStyle ls
        FilledShape fs -> setFillStyle fs

    TextForm Text { textColor = Color r g b a, .. } -> do
      Cairo.save

      layout <- Pango.createLayout textString

      Cairo.liftIO $ Pango.layoutSetAttributes layout
        [ Pango.AttrFamily { paStart = i, paEnd = j, paFamily = T.pack textTypeface }
        , Pango.AttrWeight { paStart = i, paEnd = j, paWeight = mapFontWeight textWeight }
        , Pango.AttrStyle  { paStart = i, paEnd = j, paStyle = mapFontStyle textStyle }
        , Pango.AttrSize   { paStart = i, paEnd = j, paSize = textHeight }
        ]

      Pango.PangoRectangle tx ty w h <- fmap snd $ Cairo.liftIO $ Pango.layoutGetExtents layout

      Cairo.translate ((-w / 2) - tx) ((-h / 2) - ty)
      Cairo.setSourceRGBA r g b a
      Pango.showLayout layout
      Cairo.restore

      where
        i = 0
        j = length textString

    ImageForm SDLImage { imageDims = V2 w h, .. } (V2 sx sy) (V2 sw sh) stretch -> do
      Cairo.translate (-sx) (-sy)

      if stretch then
        Cairo.scale (sw / fromIntegral w)
                    (sh / fromIntegral h)
      else
        Cairo.scale 1 1

      Cairo.setSourceSurface cairoSurface 0 0
      Cairo.translate sx sy
      Cairo.rectangle 0 0 sw sh

      if stretch then
        Cairo.paint
      else
        Cairo.fill

    GroupForm (Transform (V3 (V3 a b lx) (V3 c d ly) _)) forms -> do
      Cairo.setMatrix $ Matrix a b c d lx ly
      mapM_ renderForm forms

    CollageForm coll -> renderCollage coll

  Cairo.restore
