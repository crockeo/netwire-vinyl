{-# LANGUAGE ExistentialQuantification #-}
module Renderable where

-------------------
-- Local Imports --
import Assets
import DoList

----------
-- Code --

-- | A typeclass to define that a type can be rendered. Should only be used to
--   perform render calls.
class Renderable a where
  render :: Assets -> a -> IO ()

-- | A generic render type.
data Render = forall a. Renderable a => Render a

-- | A @'Renderable'@ instance for @'Render'@ so that after a type has been
--   wrapped, it can still be used as a @'Renderable'@ elsewhere in the code.
instance Renderable Render where
  render assets (Render a) = render assets a

-- | Being able to concatenate @'Render'@ calls through do-notation.
type Renders = DoList [Render]
