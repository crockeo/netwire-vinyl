module Food ( FoodType (..)
            , Food (..)
            , score
            , food
            ) where

--------------------
-- Global Imports --
import System.Random
import Control.Wire
import Linear.V2

----------
-- Code --

-- | The types of food.
data FoodType = Apple
              | Cherry
              | Lemon
              | Mouse

-- | An enum type to convert between ints and @'FoodType'@s.
instance Enum FoodType where
  fromEnum Apple  = 0
  fromEnum Cherry = 1
  fromEnum Lemon  = 2
  fromEnum Mouse  = 3

  toEnum 0 = Apple
  toEnum 1 = Cherry
  toEnum 2 = Lemon
  toEnum 3 = Mouse
  toEnum n = error $ "Invalid index: " ++ show n

-- | The position of the food.
data Food = Food FoodType (V2 Int)

-- | Generating a random @'FoodType'@.
randomFoodType :: IO FoodType
randomFoodType = do
  n <- randomRIO (0, 3)
  return $ toEnum n

-- | Generating a random position.
randomPosition :: IO (V2 Int)
randomPosition = do
  x <- randomIO
  y <- randomIO

  return $ V2 x y

-- | Generating a new piece of food.
randomFood :: IO Food
randomFood = do
  ft  <- randomFoodType
  pos <- randomPosition

  return $ Food ft pos

-- | Getting the score from the @'FoodType'@. Equivalent to @'fromEnum'@.
score :: FoodType -> Int
score = fromEnum

-- | The back end of the food.
food :: Food -> Wire s () IO Bool Food
food f =
  mkGen_ $ \regen -> do
    f' <- if regen
            then randomFood
            else return f

    f' `seq` return $ Right f'
