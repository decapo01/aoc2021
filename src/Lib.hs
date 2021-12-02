module Lib
    ( someFunc
    ) where

test = [
  199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263]

someFunc :: IO ()
someFunc = do
  print $ analyseMeasurements test


intFromBool :: Bool -> Int
intFromBool b = if b then 1 else 0

analyseMeasurements :: [Int] -> Int
analyseMeasurements [] = 0
analyseMeasurements (x:xs) = 
  snd $ foldl (\(prev, count) b -> (b, count + intFromBool (b > prev))) (x, 0) xs 