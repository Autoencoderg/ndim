module Data.NDim where

import Prelude

import qualified Data.Vector as V

type Dims = V.Vector Int

data NDim a = N {
  dims  :: Dims,
  nvect :: V.Vector a
  }

instance Eq a => Eq (NDim a) where
  n1 == n2 = (dims n1) == (dims n2) && (nvect n1) == (nvect n2)

instance Functor NDim where
  {-# INLINE fmap #-}
  fmap f (N d n) = N d $ V.map f n

dimProd :: Dims -> Dims
dimProd vectDims = V.tail $ V.scanr' (*) 1 vectDims

toPos :: Dims -> Dims -> Int
toPos vectDims posDims =
  let
    vectProd = dimProd vectDims
  in
    V.sum $ V.zipWith (*) vectProd posDims

fromPos :: Dims -> Int -> Dims
fromPos vectDims pos =
  let
    vectProd = dimProd vectDims
  in
    V.map fst $ V.tail $ V.scanl' (quotRem . snd) (0, pos) vectProd

unsafeGet :: Dims -> NDim a -> a
{-# INLINE unsafeGet #-}
unsafeGet posDims (N d n) = V.unsafeIndex n $ toPos d posDims 

prettyPrint :: Show a => NDim a -> String
prettyPrint nDim =
  let
    strNDim@(N d n) = fmap show nDim
    width           = V.maximum $ fmap length n
    fill     str    = replicate (width - length str) ' ' ++ str
    prefill  idx    = let brackets = V.length $ V.dropWhile (/= 0) $ V.tail idx
                      in  replicate (V.length idx - brackets) ' ' ++
                          replicate brackets '['
    postfill idx    = let brackets = V.length $ V.filter (== 1) $
                                     V.zipWith (-) (dims nDim) idx
                      in replicate brackets ']'
    blank           = fill ""
    fromIdx  idx    | fnl == V.last d = (prefill idx) ++ (fill     str) ++ ", "
                    | fnl == 1        = (fill    str) ++ (postfill idx) ++ ",\n"
                    | otherwise       = (fill    str) ++                   ", "
                    where
                      fnl = (V.last d) - (V.last idx)
                      str = unsafeGet idx strNDim
  in
    "NDim:\n" ++ ([0 .. (V.length n) - 1] >>= (fromIdx . fromPos d))
