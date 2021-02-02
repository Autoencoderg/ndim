module Data.NDim where

import Prelude
import Data.List
import Control.Monad

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

type Dims    = [Int]
type Strides = [Int]

data NDimHeader = NDH {
  dims    :: Dims,
  strides :: Strides
  }

data NDim a = ND {
  header :: NDimHeader,
  array  :: V.Vector a
  }

-- Indexing.

getStrides :: Dims -> Strides
getStrides dims' =
  tail $ scanr (*) 1 dims'

dimsToIndex :: NDimHeader -> Dims -> Int
{-# INLINE dimsToIndex #-}
dimsToIndex (NDH _ strides') dims' =
  sum $ zipWith (*) strides' dims'

indexToDims :: NDimHeader -> Int -> Dims
{-# INLINE indexToDims #-}
indexToDims (NDH _ strides') index =
  map fst $ tail $ scanl' (quotRem . snd) (0, index) strides'

-- Retrieving.

-- Unsafe get/set.
-- Used for extremely fast access by systems which are guaranteed not to fail.
-- There's no range or dimension checking.
unsafeGet :: NDim a -> Dims -> a
{-# INLINE unsafeGet #-}
unsafeGet (ND header' array') dims' =
  array' V.! (dimsToIndex header' dims')

unsafeSet :: NDim a -> Dims -> a -> NDim a
{-# INLINE unsafeSet #-}
unsafeSet (ND header' array') dims' value =
  ND header' $ V.update array' (V.singleton (dimsToIndex header' dims', value))

-- Standard get/set.
-- Will produce an error for range or dimension errors. Has more processing
-- overhead than its unsafe variant, but will have no dimension overflow.


-- Safe get/set.
-- Will return a Maybe value of the result instead of a possible error.

-- Representation.

printHeader :: NDimHeader -> String
printHeader (NDH dims' _) =
  "Dimensions: " ++ show dims' ++ ", Total Size: " ++ show size
  where
    size = product dims'

printArray :: Show a => V.Vector a -> String
printArray array'
  | length limited == length array' = show limited
  | otherwise                       = (init $ show limited) ++ ",...]"
  where
    limited = V.take 100 array'

printFull :: Show a => NDim a -> String
printFull (ND header' array') =
  "n-Dimensional array of:\n  " ++
  printHeader header' ++
  "\nWith values:\n  " ++
  printArray array'

-- Generation.

fromList :: Dims -> [a] -> NDim a
{-# INLINE fromList #-}
fromList dims' list
  | check = ND (NDH dims' (getStrides dims')) (V.fromList list)
  where
    check = length list == product dims'

genNDim :: Dims -> ([Int] -> a) -> NDim a
{-# INLINE genNDim #-}
genNDim dims' gen =
  ND header' $ V.create $ do
    vec <- MV.new $ product dims'
    let toDims = indexToDims header'
    forM_ [0 .. product dims'] (\index ->
      MV.unsafeWrite vec index $ gen (toDims index))
    return vec
  where
    header' = NDH dims' (getStrides dims')

-- Class instances.

instance Show a => Show (NDim a) where
  show ndim = printFull ndim

instance Functor NDim where
  fmap f (ND header' array') = ND header' (fmap f array')
