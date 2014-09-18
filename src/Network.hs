module Network (runNetwork) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW
import Control.Wire
import Data.IORef

-------------------
-- Local Imports --
import Renderable
import World

----------
-- Code --

-- | The backend to running the network.
runNetwork' :: (HasTime t s, Renderable b) => IORef Bool -> Session IO s -> Wire s e IO a b -> IO ()
runNetwork' closedRef session wire = do
  closed <- readIORef closedRef
  if closed
    then return ()
    else do
      (st, session') <- stepSession session
      (wt, wire'   ) <- stepWire wire st $ Right undefined

      case wt of
        Left  _     -> return ()
        Right scene -> do
          clear [ColorBuffer]
          render scene
          swapBuffers

          runNetwork' closedRef session' wire'

-- | The frontend of running the network. Really just passes everything along
--   to @'runNetwork''@ where all the work gets done.
runNetwork :: IORef Bool -> IO ()
runNetwork closedRef = runNetwork' closedRef clockSession_ worldWire
