module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.GLUtil.Camera2D
import Graphics.UI.GLFW as GLFW
import Data.Vinyl.Universe
import Control.Wire
import Data.IORef
import Data.Vinyl
import Linear.V2

-------------------
-- Local Imports --
import Renderable
import Rendering
import World

----------
-- Code --

-- | The backend to running the network.
runNetwork' :: HasTime t s => (V2 Float -> AppInfo -> IO ()) -> IORef Bool -> Camera GLfloat -> Session IO s -> Wire s e IO AppInfo World -> IO ()
runNetwork' renderFunc closedRef cam session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire wire st $ Right $ SField =: camMatrix cam

      case wt of
        Left  _     -> return ()
        Right (World ai p) -> do
          clear [ColorBuffer, DepthBuffer]
          renderFunc p ai
          swapBuffers

          runNetwork' renderFunc closedRef cam session' wire'

-- | The frontend of running the network. Really just passes everything along
--   to @'runNetwork''@ where all the work gets done.
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = do
  rc <- renderCrate
  runNetwork' rc closedRef camera2D clockSession_ worldWire
