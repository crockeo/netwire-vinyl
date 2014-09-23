module Snake ( Snake (..)
             , snake
             ) where

--------------------
-- Global Imports --
import Control.Monad (forM_)
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Assets
import Config
import Render

----------
-- Code --

-- | A type synonym for the snake. Just a list of positions within the grid.
newtype Snake = Snake [V2 Int]

-- | Allowing the @'Snake'@ to be rendered generically.
instance Renderable Snake where
  render appinfo assets (Snake l) =
    forM_ l $ \p -> do
      renderTexturedQuad (textures assets ! "white.png")
                         (shaders  assets ! "game2d"   )
                         appinfo
                         (fmap fromIntegral p * blockSize)
                         (blockSize / 2)

-- | Making the initial snake.
makeSnake :: V2 Int -> Snake
makeSnake p =
  Snake [ p
        , p - (V2 0 1)
        , p - (V2 0 2)
        ]

-- | Appending a value to the end of a @'Snake'@.
addBody :: Snake -> Snake
addBody (Snake l) = Snake $ l ++ [last l]

-- | Updating a snake in a given direction.
moveSnake :: V2 Int -> Snake -> Snake
moveSnake _   (Snake []   ) = Snake []
moveSnake dir (Snake (h:l)) =
  Snake $ (bound $ h + dir) : moveSnake' h l
  where moveSnake' :: V2 Int -> [V2 Int] -> [V2 Int]
        moveSnake' _ []     = []
        moveSnake' p (x:xs) = p : moveSnake' x xs

        bound :: V2 Int -> V2 Int
        bound (V2 x y)
          | x < 0          = V2 gridWidth y
          | x > gridWidth  = V2 0 y
          | y < 0          = V2 x gridHeight
          | y > gridHeight = V2 x 0
          | otherwise      = V2 x y

-- | The backend to the @'Snake'@ wire.
snake' :: Snake -> Wire s () IO (V2 Int, Bool) Snake
snake' s =
  mkSFN $ \(dir, add) ->
    let s' = moveSnake dir $ if add
                               then addBody s
                               else s in
      s' `seq` (s, snake' s')

-- | The wire to produce a @'Snake'@.
snake :: V2 Int -> Wire s () IO (V2 Int, Bool) Snake
snake dir = (snake' $ makeSnake $ fmap (`div` 2) gridSize) . delay (dir, False)
