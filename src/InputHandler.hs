module InputHandler ( Direction (..)
                    , inputHandler
                    , inputHandlerV
                    ) where

--------------------
-- Global Imports --
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire
import Linear.V2

-------------------
-- Local Imports --
import Input

----------
-- Code --

-- | An abstract representation of direction.
data Direction = Upwards
               | Downwards
               | Leftwards
               | Rightwards
               | NoDir

-- | Converting the @'Direction'@ to a @'V2' 'Int'@.
toV2 :: Direction -> V2 Int
toV2 Upwards    = V2 ( 0) ( 1)
toV2 Downwards  = V2 ( 0) (-1)
toV2 Leftwards  = V2 (-1) ( 0)
toV2 Rightwards = V2 ( 1) ( 0)
toV2 NoDir      = V2 ( 0) ( 0)

-- | Checking for valid @'Direction'@ changes.
changeDirection :: Direction -> Direction -> Direction
changeDirection    Upwards  Downwards = Upwards
changeDirection  Downwards    Upwards = Downwards
changeDirection  Leftwards Rightwards = Leftwards
changeDirection Rightwards  Leftwards = Rightwards
changeDirection      NoDir          d = d
changeDirection          d      NoDir = d
changeDirection          _          d = d

-- | Getting input from the user to see what the new direction should be.
newDirection :: Wire s () IO a Direction
newDirection  =  pure Upwards    . keyDown (CharKey 'W')
             <|> pure Downwards  . keyDown (CharKey 'S')
             <|> pure Leftwards  . keyDown (CharKey 'A')
             <|> pure Rightwards . keyDown (CharKey 'D')
             <|> pure NoDir

-- | The handler itself.
inputHandler :: Direction -> Wire s () IO Bool Direction
inputHandler dir =
  inputHandler' dir dir . liftA2 (,) mkId newDirection
  where inputHandler' :: Direction -> Direction -> Wire s () IO (Bool, Direction) Direction
        inputHandler' odir ndir =
          mkSFN $ \(update, ndir') ->
            if update
              then (ndir, inputHandler' ndir ndir)
              else (odir, inputHandler' odir $ changeDirection odir ndir')

-- | A @'V2' 'Int'@ version of @'inputHandler'@.
inputHandlerV :: Direction -> Wire s () IO Bool (V2 Int)
inputHandlerV = liftA toV2 . inputHandler
