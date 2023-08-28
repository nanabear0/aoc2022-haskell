import Data.Binary.Get (skip)
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

groupByElves' :: [String] -> [[Int]]
groupByElves' [] = []
groupByElves' x = map read (take splitPoint x) : groupByElves' (drop (splitPoint + 1) x)
  where
    splitPoint = fromMaybe (length x) (elemIndex "" x)

groupByElves :: String -> [[Int]]
groupByElves = groupByElves' . lines

findBiggestElf :: String -> Int
findBiggestElf x = maximum $ map sum (groupByElves x)

findNBiggestElves :: Int -> String -> Int
findNBiggestElves n list = sum $ take n $ reverse $ sort $ map sum $ groupByElves list

part1 = findBiggestElf

part2 = findNBiggestElves 3

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)
  hClose handle