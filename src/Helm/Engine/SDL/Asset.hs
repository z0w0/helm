-- | Contains the SDL asset types.
module Helm.Engine.SDL.Asset
  (
    -- * Types
    Image(..)
    -- * Loading
  , withImage
  ) where

import qualified Graphics.Rendering.Cairo as Cairo
import           Linear.V2 (V2(..))

import           Helm.Asset (Image)
import           Helm.Engine.SDL.Engine (SDLEngine)

-- | Represents an 'Image' for the SDL engine.
data instance Image SDLEngine = SDLImage
  { cairoSurface :: Cairo.Surface -- ^ The Cairo surface for the image.
  , imageDims    :: V2 Int        -- ^ The image dimensions of the image (when it was loaded).
  }

-- | Load an  image asset using the SDL engine and do
-- something with it. The image will be cleaned up
-- once the provided monad completes.
--
-- Currently, the only supported image file format is PNG.
--
-- The expected usage would be to use 'withImage'
-- for each image you need to load before
-- running the engine, and then use the images with
-- graphics. Once the engine stops running, the image
-- will then be automatically cleaned up.
withImage :: SDLEngine -> FilePath -> (Image SDLEngine -> IO a) -> IO a
withImage _ path f = Cairo.withImageSurfaceFromPNG path $ \surface -> do
  width  <- Cairo.imageSurfaceGetWidth surface
  height <- Cairo.imageSurfaceGetHeight surface

  f SDLImage
    { cairoSurface = surface
    , imageDims    = V2 width height
    }
