module World where

--------------------
-- Global Imports --
import Control.Wire

-------------------
-- Local Imports --
import Renderable

----------
-- Code --

-- | The world data type.
data World = World

-- | An instance for rendering the world.
instance Renderable World where
  render _ = return ()

-- | Constructing the final world.
worldWire :: Wire s e IO a World
worldWire = pure World
