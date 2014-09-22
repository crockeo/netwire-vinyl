module Step ( stepper
            , periodicWire
            ) where

--------------------
-- Global Imports --
import Prelude hiding ((.))
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

-- | A constant version of @'stepper'@ where it never blocks, but rather
--   returns @'True'@ when @'stepper'@ ticks.
constStepper :: (Fractional t, HasTime t s, Monoid e, Monad m) => t -> t -> Wire s e m a Bool
constStepper target t  =  pure True . stepper target t
                      <|> pure False

-- | Stepping a wire if the input is true. Steps once upon calling the
--   function, but only calls it again when provided with a @True@ input.
--   Inhibits forever when an inhibition signal is generated.
stepIf :: (Monoid s, Monoid e, Monad m) => Wire s e m a b -> Wire s e m (a, Bool) b
stepIf initWire =
  mkGen $ \ds (input, _) -> do
    (b, wire') <- stepWire initWire ds $ Right input
    case b of
      Left  b' -> return (Left  b', inhibit b')
      Right b' -> return (Right b', stepIf' b' wire')
  where stepIf' :: (Monoid s, Monoid e, Monad m) => b -> Wire s e m a b -> Wire s e m (a, Bool) b
        stepIf' buf wire =
          mkGen $ \ds (input, step) -> do
            if not step
              then return (Right buf, stepIf' buf wire)
              else do
                (b, wire') <- stepWire wire ds $ Right input
                case b of
                  Left  b' -> return (Left  b', inhibit b')
                  Right b' -> return (Right b', stepIf' b' wire')

-- | Stepping an input wire periodically. The time between updates is specified
--   exactly like in the @'stepper'@ function.
periodicWire :: (Fractional t, HasTime t s, Monoid e, Monad m) => t -> Wire s e m a b -> Wire s e m a b
periodicWire target wire = stepIf wire . liftA2 (,) mkId (constStepper target 0)