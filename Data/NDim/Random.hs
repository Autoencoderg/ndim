module Data.NDim.Random (
  randomList,
  uniform,
  normal
  ) where

import Data.NDim

import qualified System.Random as R

randomList :: (R.Random a, R.RandomGen g) =>
              (g -> (a, g)) -> g -> Int -> ([a], g)
randomList ranFunc gen len =
  (map fst full, snd $ last full)
  where
    start = ranFunc gen
    full  = take len $ iterate (ranFunc . snd) start

uniform :: (R.Random a, R.RandomGen g, R.Uniform a) =>
           g -> a -> a -> Dims -> (NDim a, g)
uniform gen lower upper dims' =
  let
    size = product dims'
    (list, gen') = randomList (R.randomR (lower, upper)) gen size
  in
    (fromList dims' list, gen')

uniformIO :: R.Uniform a => a -> a -> IO (NDim a)
uniformIO lower upper = return (fromList [0] [])

boxMuller :: Floating a => a -> a -> a
boxMuller u1 u2 =
  sqrt (-2 * log u1) * sin (2*pi * u2)

inverseZ :: Floating a => a -> a -> a -> a
inverseZ mean std z =
  (z * std) + mean

normal :: (R.Random a, R.RandomGen g, Floating a) =>
          g -> a -> a -> Dims -> (NDim a, g)
normal gen mean std dims' =
  let
    size = product dims'
    (list1, gen' ) = randomList R.random gen  size
    (list2, gen'') = randomList R.random gen' size
    normalled      = zipWith (\x y ->
                       inverseZ mean std (boxMuller x y))
                     list1 list2
  in
    (fromList dims' normalled, gen'')
