{-# LANGUAGE Arrows #-}
module World where

--------------------
-- Global Imports --
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Render
import Snake
import Food

----------
-- Code --

-- | The world data type.
data World = World Int Food Snake

instance Renderable World where
  render appinfo assets (World _ f s) = do
    render appinfo assets f
    render appinfo assets s

-- | Checking if the current @'Food'@ and @'Snake'@ collide anywhere.
overlaps :: Wire s () IO (Food, Snake) Bool
overlaps =
  mkSF_ $ \(Food _ f, Snake l) ->
    f `elem` l

-- | Checking if the @'Snake'@'s head overlaps with any other part of itself.
lose :: Wire s () IO Snake Bool
lose =
  mkSF_ $ \(Snake (h:b)) ->
    h `elem` b

-- | Handling incrementing the score.
scoreHandler :: Int -> Wire s () IO (Food, Bool) Int
scoreHandler score =
  mkSFN $ \(Food t _, inc) ->
    if not inc
      then (score, scoreHandler score)
      else
        let score' = score + fromEnum t in
          score' `seq` (score, scoreHandler score')

-- | The world wire.
worldWire :: Wire s () IO a World
worldWire =
  proc _ -> do
    rec f  <- food           -< o
        s  <- snake (V2 0 1) -< (V2 0 1, o)
        o  <- overlaps       -< (f, s)
        sc <- scoreHandler 0 -< (f, o)

    returnA -< World sc f s
