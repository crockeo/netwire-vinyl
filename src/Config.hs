module Config where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Linear.V2

----------
-- Code --

-- | The render width of the screen.
renderWidth :: Int
renderWidth = 640

-- | The render width of the screen in an OpenGL format.
glRenderWidth :: GLint
glRenderWidth = fromIntegral renderWidth

-- | The render height of the screen.
renderHeight :: Int
renderHeight = 640

-- | The render height of the screen in an OpenGL format.
glRenderHeight :: GLint
glRenderHeight = fromIntegral renderHeight

-- | The render size of the screen.
renderSize :: V2 Int
renderSize = V2 renderWidth renderHeight

-- | A float version of @'renderSize'@.
renderSizeF :: V2 Float
renderSizeF = fmap fromIntegral renderSize

-- | The width of the grid.
gridWidth :: Int
gridWidth = 20

-- | The height of the grid.
gridHeight :: Int
gridHeight = 20

-- | The size of the grid.
gridSize :: V2 Int
gridSize = V2 gridWidth gridHeight

-- | The width of a block in the grid.
blockWidth :: Float
blockWidth = (fromIntegral renderWidth) / (fromIntegral gridWidth)

-- | The height of a block in the grid.
blockHeight :: Float
blockHeight = (fromIntegral renderHeight) / (fromIntegral gridHeight)

-- | The size of a block in the grid.
blockSize :: V2 Float
blockSize = V2 blockWidth blockHeight

-- | The time between updating the game (in seconds).
updateStep :: Real a => a
updateStep = 1
