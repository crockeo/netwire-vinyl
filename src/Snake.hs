module Snake ( Snake
             , snake
             ) where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Config
import Render

----------
-- Code --

-- | A type synonym for the snake. Just a list of positions within the grid.
newtype Snake = Snake [V2 Int]

-- | Allowing the @'Snake'@ to be rendered generically.
instance Renderable Snake where
  render _ _ (Snake l) =
    mapM_ renderPos l
    where renderPos :: V2 Int -> IO ()
          renderPos p =
            let (V2 x y) = fmap fromIntegral p      :: V2 GLfloat
                (V2 w h) = fmap realToFrac gridSize :: V2 GLfloat in
              renderPrimitive Quads $
                mapM_ vertex $ Vertex2 <$> [(x * w    ) / fromIntegral renderWidth, (y * w    ) / fromIntegral renderHeight]
                                       <*> [(x * w + w) / fromIntegral renderWidth, (y * h + h) / fromIntegral renderHeight]

-- | Making the initial snake.
makeSnake :: V2 Int -> Snake
makeSnake p =
  Snake [ p
        , p - (V2 0 1)
        ]

-- | Appending a value to the end of a @'Snake'@.
addBody :: Snake -> Snake
addBody (Snake l) = Snake $ l ++ [last l]

-- | Updating a snake in a given direction.
moveSnake :: V2 Int -> Snake -> Snake
moveSnake _   (Snake []   ) = Snake []
moveSnake dir (Snake (h:l)) =
  Snake $ (h + dir) : moveSnake' h l
  where moveSnake' :: V2 Int -> [V2 Int] -> [V2 Int]
        moveSnake' _ []     = []
        moveSnake' p (x:xs) = p : moveSnake' x xs

-- | The backend to the @'Snake'@ wire.
snake' :: Snake -> Wire s () IO (V2 Int, Bool) Snake
snake' s =
  mkSFN $ \(dir, add) ->
    let s' = moveSnake dir $ if add
                               then addBody s
                               else s in
      s `seq` (s, snake' s')

-- | The wire to produce a @'Snake'@.
snake :: Wire s () IO (V2 Int, Bool) Snake
snake = snake' $ makeSnake $ fmap (`div` 2) gridSize