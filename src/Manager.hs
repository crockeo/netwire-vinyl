{-# LANGUAGE Arrows #-}
module Manager where

--------------------
-- Global Imports --
import Control.Wire
import Linear.V2

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
    step  <- constStepper updateStep 0 -< undefined
    dir   <- inputHandlerV NoDir       -< step
    world <- stepIf worldWire          -< (dir, step && dir /= V2 0 0)

    returnA -< world
