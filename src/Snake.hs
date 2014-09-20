module Snake ( Snake (..)
             , snake
             ) where

--------------------
-- Global Imports --
import Control.Wire

----------
-- Code --

-- | The datatype of the @'Snake'@.
data Snake = Snake

-- | The wire to produce a @'Snake'@.
snake :: Wire s () IO a Snake
snake = undefined