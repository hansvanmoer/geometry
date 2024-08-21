module Matrix where

import qualified AdditiveGroup as AG
import qualified Array as A
import qualified MultiplicativeGroup as MG

data (A.Array a) => Matrix a b = M Int Int (a (a b))

getSize :: (A.Array a) => Matrix a b -> (Int, Int)
getSize (M r c _) = (r, c)

setSize :: (A.Array a) => (Int, Int) -> Matrix a b -> Matrix a b
setSize (r, c) (M _ _ raster) = M r c raster

getRows :: (A.Array a) => Matrix a b -> Int
getRows (M r _ _) = r

getColumns :: (A.Array a) => Matrix a b -> Int
getColumns (M _ c _) = c

getValue :: (A.Array a) => (Int, Int) -> Matrix a b -> b
getValue (r, c) (M _ _ raster) = A.getValueAt c (A.getValueAt r raster)

setValue :: (A.Array a) => (Int, Int) -> b -> Matrix a b -> Matrix a b
setValue (r, c) v (M rows cols raster) = M rows cols (A.setValueAt r newRow raster)
  where
    row = A.getValueAt r raster
    newRow = A.setValueAt c v row

matrixFromFunction :: (A.Array a) => Matrix a b -> ((Int, Int) -> b) -> Matrix a b
matrixFromFunction input f = matrixFromFunction' input f 0 0
  where
    matrixFromFunction' :: (A.Array a) => Matrix a b -> ((Int, Int) -> b) -> Int -> Int -> Matrix a b
    matrixFromFunction' input f r c = if r == (getRows input)
                                     then input
                                     else if c == (getColumns input)
                                          then matrixFromFunction' input f (r + 1) c
                                          else matrixFromFunction' (setValue (r, c) (f (r, c)) input) f r (c + 1)

binaryByValueOperator :: (A.Array a) => Matrix a b -> Matrix a b -> (b -> b -> b) -> (Int, Int) -> b
binaryByValueOperator l r op pos = op (getValue pos l) (getValue pos r) 

unaryByValueOperator :: (A.Array a) => Matrix a b -> (b -> b) -> (Int, Int) -> b
unaryByValueOperator l op pos = op (getValue pos l)

addMatrices :: (A.Array a, AG.AdditiveGroup b) => Matrix a b -> Matrix a b -> Matrix a b
addMatrices l r = matrixFromFunction l (binaryByValueOperator l r AG.add)

subtractMatrices :: (A.Array a, AG.AdditiveGroup b) => Matrix a b -> Matrix a b -> Matrix a b
subtractMatrices l r = matrixFromFunction l (binaryByValueOperator l r AG.subtract)

multiplyMatrixAndScalar :: (A.Array a, MG.MultiplicativeGroup b) => Matrix a b -> b -> Matrix a b
multiplyMatrixAndScalar l r = matrixFromFunction l (unaryByValueOperator l (MG.multiply r))

multiplyScalarAndMatrix :: (A.Array a, MG.MultiplicativeGroup b) => b -> Matrix a b -> Matrix a b
multiplyScalarAndMatrix l r = multiplyMatrixAndScalar r l

transpose :: (A.Array a) => Matrix a b -> Matrix a b
transpose m = setSize (transposeTuple (getSize m)) (matrixFromFunction m (transposedValue m))
  where
    transposeTuple :: (Int, Int) -> (Int, Int)
    transposeTuple (x, y) = (y, x)
    transposedValue :: (A.Array a) => Matrix a b -> (Int, Int) -> b
    transposedValue m pos = getValue (transposeTuple pos) m
