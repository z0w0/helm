{-| Contains signals that sample input from the game window. -}
module FRP.Helm.Window (
  -- * Dimensions
  dimensions,
  width,
  height,
  position,
  continue
) where

import Control.Applicative (pure)
import Foreign.Marshal.Alloc
import Foreign.Storable
import FRP.Elerea.Simple hiding (Signal)
import FRP.Helm.Engine
import FRP.Helm.Sample
import FRP.Helm.Signal
import qualified Graphics.UI.SDL as SDL

{-| The current dimensions of the window. -}
dimensions :: Engine -> Signal (Int, Int)
dimensions (Engine { window }) =
  Signal $ getDimensions >>= transfer (pure (0,0)) update
  where
    getDimensions = effectful $ alloca $ \wptr -> alloca $ \hptr -> do
	    SDL.getWindowSize window wptr hptr

	    w <- peek wptr
	    h <- peek hptr

	    return (fromIntegral w, fromIntegral h)

{-| The current position of the window. -}
position :: Engine -> Signal (Int, Int)
position (Engine { window }) =
  Signal $ getPosition >>= transfer (pure (0,0)) update
  where
    getPosition = effectful $ alloca $ \xptr -> alloca $ \yptr -> do
	    SDL.getWindowPosition window xptr yptr

	    x <- peek xptr
	    y <- peek yptr

	    return (fromIntegral x, fromIntegral y)

--exposed :: Signal ()
--exposed = Signal $ getExposed
--  where
--    pass eventptr = SDL.pushEvent eventptr >> return (Unchanged ())
--    getExposed = effectful $ alloca $ \eventptr -> do
--      status <- SDL.pollEvent eventptr
--
--      if status == 1 then do
--        event <- peek eventptr
--
--        case event of
--          SDL.WindowEvent t _ _ _ _ _ -> if t == SDL.windowEventExposed
--                                         then return $ Changed ()
--                                         else pass eventptr
--          _ -> pass eventptr
--      else return $ Unchanged ()

continue :: Signal Bool
continue = Signal $ getQuit
  where
    getQuit = effectful $ alloca $ \eventptr -> do
      status <- SDL.pollEvent eventptr

      if status == 1 then do
        event <- peek eventptr

        case event of
          SDL.QuitEvent _ _ -> return $ Changed False
          _ -> return $ Unchanged True
      else return $ Unchanged True

{-| The current width of the window. -}
width :: Engine -> Signal Int
width engine = fst <~ dimensions engine

{-| The current height of the window. -}
height :: Engine -> Signal Int
height engine = snd <~ dimensions engine
