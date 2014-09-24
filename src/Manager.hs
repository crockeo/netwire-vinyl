{-# LANGUAGE Arrows #-}
module Manager where

--------------------
-- Global Imports --
import Control.Wire

-------------------
-- Local Imports --
import InputHandler
import Config
import World
import Step

----------
-- Code --

-- | Managing the world and input manager.
manager :: (Fractional t, HasTime t s) => Wire s () IO a World
manager =
  proc _ -> do
    step  <- constStepper 0 updateStep -< undefined
    dir   <- inputHandlerV Upwards     -< step
    world <- stepIf worldWire          -< (dir, step)

    returnA -< world
