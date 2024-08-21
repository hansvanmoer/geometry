module Vector where

import qualified AdditiveGroup as AG
import qualified Array as A
import qualified MultiplicativeGroup as MG

getDimension :: (A.Array a) => a b -> Int
getDimension = A.getSize

-- Gets an element of a vector
getCoordinate :: (A.Array a) => Int -> a b -> b
getCoordinate = A.getValueAt

-- Gets an element of a vector
setCoordinate :: (A.Array a) => Int -> b -> a b -> a b
setCoordinate = A.setValueAt

-- Adds two vectors
addVectors :: (A.Array a, AG.AdditiveGroup b) => a b -> a b -> a b
addVectors l r = pure AG.add <*> l <*> r

-- Subtracts two vectors
subtractVectors :: (A.Array a, AG.AdditiveGroup b) => a b -> a b -> a b
subtractVectors l r = pure AG.subtract <*> l <*> r

-- Inverts a vector
invertVector :: (A.Array a, AG.AdditiveGroup b) => a b -> a b
invertVector v = fmap AG.invert v

-- Multiplies a vector and a scalar
multiplyVectorAndScalar :: (A.Array a, MG.MultiplicativeGroup b) => a b -> b -> a b
multiplyVectorAndScalar l r = fmap (MG.multiply r) l

-- Multiplies a scalar and a vector
multiplyScalarAndVector :: (A.Array a, MG.MultiplicativeGroup b) => b  -> a b -> a b
multiplyScalarAndVector l r  = multiplyVectorAndScalar r l

-- Divides a vector by a scalar
divideVectorByScalar :: (A.Array a, MG.MultiplicativeGroup b) => a b -> b -> a b
divideVectorByScalar l r = multiplyVectorAndScalar l (MG.invert r)

-- The zero vector
zeroVector :: (A.Array a, AG.AdditiveGroup b) => a b
zeroVector = A.uniform AG.identity

-- The unit vector
unitVector :: (A.Array a, MG.MultiplicativeGroup b) => a b
unitVector = A.uniform MG.identity

-- The inner product of two vectors
innerProduct :: (A.Array a, AG.AdditiveGroup b, MG.MultiplicativeGroup b) => a b -> a b -> b
innerProduct l r = foldr AG.add AG.identity (pure MG.multiply <*> l <*> r)

-- The norm of the vector according to the standard definition
norm :: (A.Array a, AG.AdditiveGroup b, MG.MultiplicativeGroup b, Floating b) => a b -> b
norm v = sqrt (innerProduct v v)
