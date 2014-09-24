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
import Manager
import Assets
import Render

----------
-- Code --

-- | Loading the project's assets.
loadAssets :: AssetLoads
loadAssets = do
  loadTexture "white.png"
  loadTexture "apple.png"
  loadTexture "cherry.png"
  loadTexture "crate.png"
  loadTexture "lemon.png"
  loadTexture "mouse.png"
  loadTexture "white.png"

  loadShaderProgram "game2d"

-- | The backend to the @'runNetwork'@ function.
runNetwork' :: (Fractional t, HasTime t s, Renderable b) => IORef Bool -> Assets -> Camera GLfloat -> Session IO s -> Wire s e IO a b -> IO ()
runNetwork' closedRef assets cam session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (ds, session') <- stepSession session
      (dw, wire'   ) <- stepWire wire ds $ Right undefined

      case dw of
        Left  _   -> return ()
        Right dw' -> do
          clear [ColorBuffer, DepthBuffer]
          render (SField =: camMatrix cam) assets dw'
          swapBuffers

          runNetwork' closedRef assets cam session' wire'

-- | The frontend of running the network. Really just passes everything along
--   to @'runNetwork''@ where all the work gets done.
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = do
  assets <- performAssetLoads loadAssets
  runNetwork' closedRef assets camera2D clockSession_ manager
