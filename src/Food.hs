module Food (food) where

--------------------
-- Global Imports --
import System.Random
import Control.Wire
import Linear.V2

----------
-- Code --

-- | The position of the food.
newtype Food = Food (V2 Int)

-- | Generating a random position.
randomPosition :: IO (V2 Int)
randomPosition = do
  x <- randomIO
  y <- randomIO

  return $ V2 x y

-- | Generating the position of the food.
food :: V2 Int -> Wire s () IO Bool Food
food pos =
  mkGen_ $ \regen -> do
    np <- if regen
            then randomPosition
            else return pos

    return $ Right $ Food np
