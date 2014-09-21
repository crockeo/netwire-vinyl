module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.GLUtil.Camera2D
import Graphics.UI.GLFW as GLFW
import Data.Vinyl.Universe
import Control.Concurrent
import Control.Wire
import Data.IORef
import Data.Vinyl

-------------------
-- Local Imports --
import Assets
import Render
import World

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

-- | The loop for updating the game.
updateGame :: Session IO s -> Wire s e IO a World -> IORef (Maybe World) -> IORef Bool -> IO ()
updateGame session wire worldRef closedRef = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire wire st $ Right undefined

      case wt of
        Left  _     -> atomicWriteIORef closedRef True
        Right world -> do
          atomicWriteIORef worldRef $ Just world
          threadDelay 1000000
          updateGame session' wire' worldRef closedRef

-- | The loop for rendering the game.
renderGame :: IORef (Maybe World) -> IORef Bool -> Assets -> Camera GLfloat -> IO ()
renderGame worldRef closedRef assets cam = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      mworld <- readIORef worldRef

      case mworld of
        Nothing    -> renderGame worldRef closedRef assets cam
        Just world -> do
          clear [ColorBuffer, DepthBuffer]
          render (SField =: camMatrix cam) assets world
          swapBuffers

          renderGame worldRef closedRef assets cam

-- | The frontend of running the network. Really just passes everything along
--   to @'runNetwork''@ where all the work gets done.
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = do
  worldRef <- newIORef Nothing
  assets   <- performAssetLoads loadAssets

  forkIO $ updateGame clockSession_ worldWire worldRef closedRef
  renderGame worldRef closedRef assets camera2D
