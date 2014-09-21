module Step (worldStepper) where

--------------------
-- Global Imports --
import Control.Wire

-------------------
-- Local Imports --
import World

----------
-- Code --

-- | A wire to update the world periodically. The period between updates is
--   specified by @'updateStep'@ in the Config module.
worldStepper :: Wire s () IO a World
worldStepper = undefined