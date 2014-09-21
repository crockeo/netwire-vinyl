module Step ( stepper
            , periodicWire
            ) where

--------------------
-- Global Imports --
import Control.Wire

----------
-- Code --

-- | A wire that blocks until the specified time has been met. At that point it
--   results in the input type, and sets it counter back to zero.
stepper :: (Fractional t, HasTime t s, Monoid e) => t -> t -> Wire s e m a a
stepper target t =
  mkPure $ \ds a ->
    let dt = realToFrac $ dtime ds
        t' = t + dt in
      t' `seq` if t > target
                 then (Right a     , stepper target 0 )
                 else (Left  mempty, stepper target t')

-- | Stepping an input wire periodically. The time between updates is specified
--   exactly like in the @'stepper'@ function.
periodicWire :: t -> Wire s e m a b -> Wire s e m a b
periodicWire _ _ = undefined