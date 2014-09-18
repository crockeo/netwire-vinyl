module DoList where

--------------------
-- Global Imports --
import Control.Applicative
import Data.Monoid

----------
-- Code --

-- | The base-data type for concatenating lists (and other Monoids) through
--   do-notation.
data DoListT a b = DoListT a b

-- | A functor instance to satisfy the Applicative instance.
instance Functor (DoListT a) where
  fmap fn (DoListT a b) = DoListT a $ fn b

-- | An Applicative instance primarily to satisfy the Monad's
--   future-requirement. When Applicative-do is released, I believe it should
--   function properly in that contex as well.
instance Monoid a => Applicative (DoListT a) where
  pure b = DoListT mempty b
  (DoListT a' fn) <*> (DoListT a b) =
    DoListT (mappend a a') $ fn b

-- | The main sauce of the @'DoListT'@. Allows the user to concatenate Monoid
--   values through do-notation.
instance Monoid a => Monad (DoListT a) where
  return = pure
  (DoListT a b) >>= fn =
    let (DoListT a' b') = fn b in
      DoListT (mappend a a') b'

-- | A convenient version of @'DoListT'@ where the @b@ value is simply @()@.
type DoList a = DoListT a ()

-- | Converting a @[]@-kind of list to a @'DoList'@.
dlFromList :: [a] -> DoList [a]
dlFromList l = DoListT l ()

-- | Converting a @'DoList'@ to a @[]@-kind of list.
dlToList :: DoList [a] -> [a]
dlToList (DoListT l _) = l
