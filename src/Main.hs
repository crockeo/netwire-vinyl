module Main where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Data.IORef

-------------------
-- Local Imports --
import Network

----------
-- Code --

-- | The callback for when the window should be closed.
makeWindowCloseCallback :: IORef Bool -> WindowCloseCallback
makeWindowCloseCallback closedRef = do
  writeIORef closedRef True
  return True

-- | The callback for when the window is resized.
makeWindowSizeCallback :: WindowSizeCallback
makeWindowSizeCallback s@(Size w h) = do
  viewport $= (Position 0 0, s)

  matrixMode $= Projection
  loadIdentity

  ortho 0 (fromIntegral w) (fromIntegral h) 0 (-1) 1

  matrixMode $= Modelview 0

-- | The entry point to the program. Sets things up and passes it off to the
--   Netwire network.
main :: IO ()
main = do
  initialize
  openWindow (Size 640 480) [DisplayRGBBits 8 8 8, DisplayAlphaBits 8, DisplayDepthBits 24] Window
  windowTitle $= "netwire-vinyl"

  closedRef <- newIORef False
  windowCloseCallback $= do
    writeIORef closedRef True
    return True

  windowCloseCallback $= makeWindowCloseCallback closedRef
  windowSizeCallback  $= makeWindowSizeCallback

  runNetwork closedRef

  closeWindow
