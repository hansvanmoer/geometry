module MultiplicativeGroup (MultiplicativeGroup, divide, identity, invert, multiply) where

-- A multiplicative group
class MultiplicativeGroup a where
  -- Divide two elements
  divide ::a -> a -> a
  divide l r = multiply l (invert r)
  -- The identity element for multiplication
  identity :: a
  -- Inversion under multiplication
  invert :: a -> a
  -- Multiplies two elements in the group
  multiply :: a -> a -> a
