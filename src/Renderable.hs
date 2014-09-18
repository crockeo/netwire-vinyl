{-# LANGUAGE ExistentialQuantification #-}
module Renderable where

--------------------
-- Global Imports --
import Control.Applicative

-------------------
-- Local Imports --
import DoList

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

-- | Being able to concatenate @'Render'@ calls through do-notation.
type Renders = DoList [Render]
