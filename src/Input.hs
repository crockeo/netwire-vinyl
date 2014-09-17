module Input where

--------------------
-- Global Imports --
import Graphics.Rendering.OpenGL
import Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire

----------
-- Code --

-- | Getting the current mouse position (raw value from GLFW).
getMousePosition :: Wire s e IO a Position
getMousePosition =
  mkGen_ $ \_ -> do
    pos <- get mousePos
    return $ Right pos

-- | Checking if a given key is pressed.
keyDown :: (Enum k, Monoid e) => k -> Wire s e IO a a
keyDown key =
  mkGen_ $ \a -> do
    state <- getKey key
    return $ case state of
      Release -> Left  mempty
      Press   -> Right a

-- | An always-returning @'Bool'@ value to check if a key is down.
isKeyDown :: (Enum k, Monoid e) => k -> Wire s e IO a Bool
isKeyDown k  =  pure True  . keyDown k
            <|> pure False

-- | Checking if a mouse button is down.
mouseButtonDown :: Monoid e => MouseButton -> Wire s e IO a a
mouseButtonDown button =
  mkGen_ $ \a -> do
    state <- getMouseButton button
    return $ case state of
      Release -> Left  mempty
      Press   -> Right a

-- | An always-returning @'Bool'@ valeu to check if a mouse button is down.
isMouseButtonDown :: Monoid e => MouseButton -> Wire s e IO a Bool
isMouseButtonDown b  =  pure True  . mouseButtonDown b
                    <|> pure False
