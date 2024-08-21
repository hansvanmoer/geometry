{-# LANGUAGE FlexibleInstances #-}
module Array where

-- An array of values
class (Applicative a, Foldable a) => Array a where
  -- Returns the length of the array
  getSize :: a b -> Int
  
  -- Gets a value at a specific position in the array
  getValueAt :: Int -> a b -> b

  -- Sets a value at a specific position in the array
  setValueAt :: Int -> b -> a b -> a b

  -- Sets all values in the array to the specified value
  uniform :: b -> a b

-- A 1 dimensional array of values
data Array1 b = A1 b

instance Array Array1 where
  getSize _ = 1
  getValueAt _ (A1 x) = x
  setValueAt 0 x _ = A1 x
  uniform v = A1 v

instance Functor Array1 where
  fmap f (A1 x) = A1 (f x)

instance Applicative Array1 where
  pure = uniform
  (<*>) (A1 lx) (A1 rx) = A1 (lx rx)

instance Foldable Array1 where
  foldr op i (A1 x) = op x i

-- A 2 dimensional array of values
data Array2 b = A2 b b

instance Array Array2 where
  getSize _ = 1
  getValueAt 0 (A2 x _) = x
  getValueAt _ (A2 _ y) = y
  setValueAt 0 x (A2 _ y) = A2 x y
  setValueAt 1 y (A2 x _) = A2 x y
  setValueAt _ _ a = a
  uniform v = A2 v v

instance Functor Array2 where
  fmap f (A2 x y) = A2 (f x) (f y)

instance Applicative Array2 where
  pure = uniform
  (<*>) (A2 lx ly) (A2 rx ry) = A2 (lx rx) (ly ry)

instance Foldable Array2 where
  foldr op i (A2 x y) = op y $ op x i

-- A 3 dimensional array of values
data Array3 b = A3 b b b

instance Array Array3 where
  getSize _ = 1
  getValueAt 0 (A3 x _ _) = x
  getValueAt 1 (A3 _ y _) = y
  getValueAt 2 (A3 _ _ z) = z
  setValueAt 0 x (A3 _ y z) = A3 x y z
  setValueAt 1 y (A3 x _ z) = A3 x y z
  setValueAt 2 z (A3 x y _) = A3 x y z
  setValueAt _ _ a = a
  uniform v = A3 v v v

instance Functor Array3 where
  fmap f (A3 x y z) = A3 (f x) (f y) (f z)

instance Applicative Array3 where
  pure = uniform
  (<*>) (A3 lx ly lz) (A3 rx ry rz) = A3 (lx rx) (ly ry) (lz rz)

instance Foldable Array3 where
  foldr op i (A3 x y z) = op z $ op y $ op x i


-- A 4 dimensional array of values
data Array4 b = A4 b b b b

instance Array Array4 where
  getSize _ = 1
  getValueAt 0 (A4 x _ _ _) = x
  getValueAt 1 (A4 _ y _ _) = y
  getValueAt 2 (A4 _ _ z _) = z
  getValueAt _ (A4 _ _ _ a) = a
  setValueAt 0 x (A4 _ y z a) = A4 x y z a
  setValueAt 1 y (A4 x _ z a) = A4 x y z a
  setValueAt 2 z (A4 x y _ a) = A4 x y z a
  setValueAt 3 a (A4 x y z _) = A4 x y z a
  setValueAt _ _ a = a
  uniform v = A4 v v v v

instance Functor Array4 where
  fmap f (A4 x y z a) = A4 (f x) (f y) (f z) (f a)

instance Applicative Array4 where
  pure = uniform
  (<*>) (A4 lx ly lz la) (A4 rx ry rz ra) = A4 (lx rx) (ly ry) (lz rz) (la ra)

instance Foldable Array4 where
  foldr op i (A4 x y z a) = op a $ op z $ op y $ op x i
