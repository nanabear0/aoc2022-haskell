import Data.Binary.Get (skip)
import Data.List (elemIndex, sort)
import Data.Maybe (fromMaybe)
import System.IO (readFile)

groupByElves' :: [String] -> [[Int]]
groupByElves' [] = []
groupByElves' x = (map read . take splitPoint) x : (groupByElves' . drop (splitPoint + 1)) x
  where
    splitPoint = fromMaybe (length x) (elemIndex "" x)

groupByElves :: String -> [[Int]]
groupByElves = groupByElves' . lines

findBiggestElf :: String -> Int
findBiggestElf = maximum . map sum . groupByElves

findNBiggestElves :: Int -> String -> Int
findNBiggestElves n = sum . take n . reverse . sort . map sum . groupByElves

part1 = findBiggestElf

part2 = findNBiggestElves 3

main = do
  contents <- readFile "input.txt"
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)