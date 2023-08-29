import Data.Char (isDigit, isSpace)
import Data.List (elemIndex, transpose, unfoldr)
import Data.Maybe (fromMaybe)
import System.IO (readFile)

swap2 :: [String] -> Int -> Int -> String -> String -> [String]
swap2 [] _ _ _ _ = []
swap2 (ns : state) 0 y nx ny = nx : swap2 state (-1) (y - 1) nx ny
swap2 (ns : state) x 0 nx ny = ny : swap2 state (x - 1) (-1) nx ny
swap2 (ns : state) x y nx ny = ns : swap2 state (x - 1) (y - 1) nx ny

updateState :: Bool -> [String] -> Int -> Int -> Int -> [String]
updateState insertInReverse state n s t = swap2 state (s - 1) (t - 1) newSource newTarget
  where
    sourceStack = state !! (s - 1)
    targetStack = state !! (t - 1)
    newSource = drop n sourceStack
    newTarget = (if insertInReverse then reverse else id) (take n sourceStack) ++ targetStack

runOp :: Bool -> ([String], [(Int, Int, Int)]) -> [String]
runOp _ (x, []) = x
runOp insertInReverse (x, (n, s, t) : rest) = runOp insertInReverse (updateState insertInReverse x n s t, rest)

readOperations :: [String] -> [(Int, Int, Int)]
readOperations x = toTuple . map read . filter (all isDigit) . words <$> x
  where
    toTuple [x, y, z] = (x, y, z)

readState :: [String] -> [String]
readState = map (filter (not . isSpace)) . transpose . map (map (!! 1) . chunks 4)
  where
    chunks n = takeWhile (not . null) . unfoldr (Just . splitAt n)

readInput :: String -> ([String], [(Int, Int, Int)])
readInput x = (readState $ init firstPart, readOperations $ tail secondPart)
  where
    l = lines x
    separator = fromMaybe (length l) (elemIndex "" l)
    (firstPart, secondPart) = splitAt separator l

readTops :: [String] -> String
readTops = map head

part1 :: String -> String
part1 = readTops . runOp True . readInput

part2 :: String -> String
part2 = readTops . runOp False . readInput

main = do
  contents <- readFile "input.txt"
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)