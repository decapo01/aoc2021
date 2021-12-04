module Lib
    ( someFunc
    ) where
import System.IO
import Data.Char (isSpace)
import qualified Data.Char as Char

someFunc :: IO ()
someFunc = do
  measurementsHandle <- readFile "./data/measurements" 
  movementsHandle <- readFile "./data/movements" 
  let measurements = fmap (\x -> read x :: Int) (lines measurementsHandle)
  let movements = movementsFromStr <$> lines movementsHandle
  print $ analyseMeasurements measurements 
  let pos = simulateMovements movements
  print $ "Position Mult: " ++ show (horz pos * depth pos)


intFromBool :: Bool -> Int
intFromBool b = if b then 1 else 0

analyseMeasurements :: [Int] -> Int
analyseMeasurements [] = 0
analyseMeasurements (x:xs) = 
  snd $ foldl (\(prev, count) b -> (b, count + intFromBool (b > prev))) (x, 0) xs 


data SubMovements
  = Forward Int
  | Down Int
  | Up Int

movementsFromStr :: String -> SubMovements
movementsFromStr x =
  case m of
    "forward" -> Forward val
    "up"      -> Up val
    "down"    -> Down val
    _ -> Down val -- Should never get here
  where
    [m, v] = split Char.isSpace x
    val = read v :: Int

split :: (Char -> Bool) -> String -> [String]
split p s =
  case dropWhile p s of
    "" -> []
    s' -> w : words s''
          where (w, s'') = break Char.isSpace s'


data Position = Position { horz :: Int , depth :: Int } deriving (Show)

move :: SubMovements -> Position -> Position
move mvnt state = case mvnt of
  Forward n -> state { horz = horz state + n }
  Down n -> state { depth = depth state + n }
  Up n -> state { depth = depth state - n }
    
simulateMovements :: [SubMovements] -> Position
simulateMovements =
  foldl (flip move) (Position 0 0) 