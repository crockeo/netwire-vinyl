{-# LANGUAGE Arrows #-}
module World where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL hiding (position)
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Linear.V2

-------------------
-- Local Imports --
import Renderable
import Rendering
import Assets
import Input

----------
-- Code --

-- | The world data type.
data World = World AppInfo (V2 Float) (V2 Float)

instance Renderable World where
  render assets (World info p s) = do
    (Size w' h') <- get windowSize
    let si = V2 (realToFrac w') (realToFrac h') :: V2 Float
    renderTexturedQuad (textures assets ! "crate.png")
                       (shaders  assets ! "game2d"   )
                       info
                       (p / si)
                       (s / si)

-- | The speed of the crate.
speed :: Float
speed = 512

-- | Moving in a given direction.
direction :: Enum k => k -> k -> Wire s () IO a Float
direction k1 k2  =  pure 0        . keyDown k1 . keyDown k2
                <|> pure (-speed) . keyDown k1
                <|> pure ( speed) . keyDown k2
                <|> pure 0

-- | The speed of the crate.
biDirection :: Wire s () IO (Maybe a) (V2 Float)
biDirection = liftA2 V2 (direction (CharKey 'A') (CharKey 'D'))
                        (direction (CharKey 'S') (CharKey 'W'))

-- | The position of the crate.
position :: HasTime t s => Wire s () IO (V2 Float) (V2 Float)
position = integral 320

-- | The size of the quad.
size :: Wire s () IO a (V2 Float)
size = pure $ pure 128

-- | Constructing the final world.
worldWire :: HasTime t s => Wire s () IO AppInfo World
worldWire =
  proc ai -> do
    bd <- biDirection -< Nothing
    p  <- position    -< bd
    s  <- size        -< undefined

    returnA -< World ai p s
