module Main where

import Control.Parallel.Strategies

f :: Double -> Double
f x = x

calculateTrapezoidal :: Double -> Double -> Int -> Double
calculateTrapezoidal a b n = h * (0.5 * (f a + f b) + sumTerms)
  where
    h = (b - a) / fromIntegral n
    sumTerms = sum [f (a + fromIntegral i * h) | i <- [1 .. n - 1]]

calculateIntegral :: Double -> Double -> Int -> Int -> Double
calculateIntegral a b n numThreads =
  result `using` rseq
  where
    subintervals = [(a + fromIntegral i * h, a + fromIntegral (i + 1) * h) | i <- [0 .. numThreads - 1]]
    results = map (\(start, end) -> calculateTrapezoidal start end n) subintervals
    result = sum results
    h = (b - a) / fromIntegral numThreads

main :: IO ()
main = do
  let a = 1.0
      b = 10.0
      n = 1000000
      numThreads = 4
  print (calculateIntegral a b n numThreads)
