module Data.NDim where

import Prelude
import Data.List
import Control.Monad

import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

--------------------------------------------------------------------------------
-- DATA TYPES

-- Improves readability.
type Dims    = [Int]
type Strides = [Int]

-- The header inside an nDim array.
data NDimHeader = NDH {
  dims    :: Dims,
  strides :: Strides
  }

-- The nDim array itself.
data NDim a = ND {
  header :: NDimHeader,
  array  :: V.Vector a
  }

--------------------------------------------------------------------------------
-- INDEXING

-- Produce the list of strides for the specified dimensions.
getStrides :: Dims -> Strides
getStrides dims' =
  tail $ scanr (*) 1 dims'

-- Convert a dimension index to a flat index for the raw data vector.
dimsToIndex :: NDimHeader -> Dims -> Int
{-# INLINE dimsToIndex #-}
dimsToIndex (NDH _ strides') dims' =
  sum $ zipWith (*) strides' dims'

-- Convert a flat index into a dimension index.
indexToDims :: NDimHeader -> Int -> Dims
{-# INLINE indexToDims #-}
indexToDims (NDH _ strides') index =
  map fst $ tail $ scanl' (quotRem . snd) (0, index) strides'

--------------------------------------------------------------------------------
-- RETRIEVING

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

--------------------------------------------------------------------------------
-- REPRESENTATION

-- Produce a string representing an nDim array header.
printHeader :: NDimHeader -> String
printHeader (NDH dims' _) =
  "Dimensions: " ++ show dims' ++ ", Total Size: " ++ show size
  where
    size = product dims'

-- Produce a string representing the raw data in an nDim array.
printArray :: Show a => V.Vector a -> String
printArray array'
  | length limited == length array' = show limited
  | otherwise                       = (init $ show limited) ++ ",...]"
  where
    limited = V.take 20 array'

-- Produce a string representing a full nDim array with header and data.
printFull :: Show a => NDim a -> String
printFull (ND header' array') =
  "n-Dimensional array of:\n  " ++
  printHeader header' ++
  "\nWith values:\n  " ++
  printArray array'

--------------------------------------------------------------------------------
-- VALIDATION

-- Check that two nDim arrays have the same dimensions.
sameDims :: NDim a -> NDim b -> Bool
sameDims (ND (NDH dims1 _) _) (ND (NDH dims2 _) _) =
  and $ zipWith (==) dims1 dims2

-- Check that every nDim array in a list is of the same dimensions.
sameDimsInList :: [NDim a] -> Bool
sameDimsInList ndims
  | length ndims <= 1 = True
  | current           = sameDimsInList (x2:xs)
  | otherwise         = False
  where
    (x1:x2:xs) = ndims
    current    = sameDims x1 x2

-- Check that every nDim array in an nDim array is of the same dimensions.
allSameDims :: NDim (NDim a) -> Bool
allSameDims (ND _ ndims) =
  let
    check = (V.and .) . V.zipWith sameDims
  in
    check ndims (V.tail ndims)

--------------------------------------------------------------------------------
-- GENERATION

-- Produce an nDim array from the dimensions specified and the values to fit
-- that. The length of the list must equal the product of the dimensions.
fromList :: Dims -> [a] -> NDim a
{-# INLINE fromList #-}
fromList dims' list
  | check     = ND (NDH dims' (getStrides dims')) (V.fromList list)
  | otherwise = error "Length of list does not match dimensions provided."
  where
    check = length list == product dims'

-- Generate an nDim array from the dimensions specified and a generator
-- function that takes each successive index to produce the value at that
-- index.
-- Note: nDim indexing sets the final dimension as the most frequently changing
-- index.
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

--------------------------------------------------------------------------------
-- MANIPULATION

-- Collapses an nDim array of nDim arrays into one nDim array provided that
-- every nDim array is of the same dimensions.
collapse :: NDim (NDim a) -> NDim a
collapse ndim@(ND header' array')
  | check     = ND newHeader $ V.concat arrays
  | otherwise = error "All nDim arrays in the nDim array must be of the same \
                     \ dimensions."
  where
    check     = allSameDims ndim
    newDims   = dims header' ++ (dims . header . V.head) array'
    newHeader = NDH newDims (getStrides newDims)
    arrays    = [array (array' V.! x) | x <- [0 .. length array' - 1]]

-- Flattens an nDim array of nDim arrays into one nDim array based on the
-- maximum dimensions of the subarrays, padding nonexistent values with
-- Nothing.
-- flatten :: NDim (NDim a) -> NDim (Maybe a)
-- flatten ndim = ND (NDH [1] [1]) (V.singleton (Just 0))
  
--------------------------------------------------------------------------------
-- CLASS INSTANCES

instance Show a => Show (NDim a) where
  show ndim = printFull ndim

instance Functor NDim where
  fmap f (ND header' array') = ND header' (fmap f array')

{-
instance Applicative NDim where
  pure x    = fromList [1] $ [x]
  fn <*> xn = collapse $ fmap (\f -> fmap f xn) fn
-}
