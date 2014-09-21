{-# LANGUAGE Arrows #-}
module World where

--------------------
-- Global Imports --
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Render
import Snake
import Food

----------
-- Code --

-- | The world data type.
data World = World Food Snake

instance Renderable World where
  render appinfo assets (World f s) = do
    render appinfo assets f
    render appinfo assets s

-- | The world wire.
worldWire :: Wire s () IO a World
worldWire =
  proc _ -> do
    f <- food  -< False
    s <- snake -< (V2 0 0, False)

    returnA -< World f s
