module AdditiveGroup (AdditiveGroup, add, identity, invert, subtract) where

import Prelude hiding (subtract)

-- An additive group with inversion
class AdditiveGroup a where
  -- Adds two members of the group
  add :: a -> a -> a
  -- The identity for the addition operator
  identity :: a
  -- Inverts an elememt under addition
  invert :: a -> a
  -- Subtracts two elements
  subtract :: a -> a -> a
  subtract l r = add l (invert r)

