{-# LANGUAGE ExistentialQuantification #-}
module Renderable where

--------------------
-- Global Imports --
import Control.Applicative

----------
-- Code --

-- | A typeclass to define that a type can be rendered. Should only be used to
--   perform render calls.
class Renderable a where
  render :: a -> IO ()

-- | A generic render type.
data Render = forall a. Renderable a => Render a

-- | A @'Renderable'@ instance for @'Render'@ so that after a type has been
--   wrapped, it can still be used as a @'Renderable'@ elsewhere in the code.
instance Renderable Render where
  render (Render a) = render a

-- | A do-composable list of render calls.
data RendersT a = RendersT [Render] a

-- | The commonly used version of @'RendersT'@.
type Renders = RendersT ()

-- | Maps over the value contained in a @'RendersT'@.
instance Functor RendersT where
  fmap fn (RendersT l a) = RendersT l $ fn a

-- | Applies composable functions to a @'RendersT'@. In the event that the
--   function contains a list, it will be appended to the list in the applied
--   value.
instance Applicative RendersT where
  pure a = RendersT [] a
  (RendersT l' fn) <*> (RendersT l a) =
    RendersT (l ++ l') $ fn a

-- | The main sauce of the @'RendersT'@. This allows @'Render'@ calls to be
--   composd through do-notation. So one can purely emulate IO-based rendering
--   and then just render everything in the end.
instance Monad RendersT where
  return = pure
  (RendersT l a) >>= fn =
    let (RendersT l' a') = fn a in
      RendersT (l ++ l') a'

-- | Adding a @'Renderable'@ instance to @'RendersT'@ so that after composition
--   it may be used to render very easily.
instance Renderable (RendersT a) where
  render (RendersT l _) = mapM_ render l
