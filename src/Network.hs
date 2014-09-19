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

-------------------
-- Local Imports --
import Renderable
import Rendering
import Assets
import World

----------
-- Code --

-- | Loading the project's assets.
loadAssets :: AssetLoads
loadAssets = do
  loadTexture       "crate.png"
  loadShaderProgram "game2d"

-- | The backend to running the network.
runNetwork' :: HasTime t s => IORef Bool -> Assets -> Camera GLfloat -> Session IO s -> Wire s e IO AppInfo World -> IO ()
runNetwork' closedRef assets cam session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire wire st $ Right $ SField =: camMatrix cam

      case wt of
        Left  _     -> return ()
        Right world -> do
          clear [ColorBuffer, DepthBuffer]
          render assets world
          swapBuffers

          runNetwork' closedRef assets cam session' wire'

-- | The frontend of running the network. Really just passes everything along
--   to @'runNetwork''@ where all the work gets done.
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = do
  assets <- performAssetLoads loadAssets
  runNetwork' closedRef assets camera2D clockSession_ worldWire
