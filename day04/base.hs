import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import System.IO (readFile)

toRangeNumTuple :: (String, String) -> (Integer, Integer)
toRangeNumTuple (x, y) = (read x :: Integer, read $ tail y :: Integer)

toRangeTuple :: String -> (String, String)
toRangeTuple x = splitAt dashIndex x
  where
    dashIndex = fromMaybe (length x) (elemIndex '-' x)

toRangesPair :: (String, String) -> ((Integer, Integer), (Integer, Integer))
toRangesPair (x, y) = (helper x, helper $ tail y)
  where
    helper = toRangeNumTuple . toRangeTuple

splitToRanges :: String -> (String, String)
splitToRanges x = splitAt commaIndex x
  where
    commaIndex = fromMaybe (length x) (elemIndex ',' x)

rangeFullyContainsOther :: ((Integer, Integer), (Integer, Integer)) -> Bool
rangeFullyContainsOther ((l0, h0), (l1, h1)) = (l0 - l1) * (h1 - h0) >= 0

rangesOverlap :: ((Integer, Integer), (Integer, Integer)) -> Bool
rangesOverlap ((l0, h0), (l1, h1)) = h0 >= l1 && h1 >= l0

contentsToRangeTuples :: String -> [((Integer, Integer), (Integer, Integer))]
contentsToRangeTuples = map (toRangesPair . splitToRanges) . lines

part1 :: String -> Int
part1 = length . filter rangeFullyContainsOther . contentsToRangeTuples

part2 :: String -> Int
part2 = length . filter rangesOverlap . contentsToRangeTuples

main = do
  contents <- readFile "input.txt"
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)