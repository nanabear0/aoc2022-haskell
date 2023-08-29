import Data.Char (isUpper)
import Data.List (intersect, nub)
import System.IO (readFile)

-- shared
toPriority :: Char -> Int
toPriority x = fromEnum x - lowerBase
  where
    lowerBase = if isUpper x then fromEnum 'A' - 27 else fromEnum 'a' - 1

allItemsToPriority :: [Char] -> Int
allItemsToPriority = sum . map toPriority

-- part1
itemsInBothCompartments :: Eq a => ([a], [a]) -> [a]
itemsInBothCompartments (x, y) = nub x `intersect` y

splitToRucksacks :: String -> [([Char], [Char])]
splitToRucksacks = map (\y -> splitAt (length y `div` 2) y) . lines

part1 :: String -> Int
part1 = sum . map (allItemsToPriority . itemsInBothCompartments) . splitToRucksacks

-- part2
itemsInAllElves :: Eq a => ([a], [a], [a]) -> [a]
itemsInAllElves (x, y, z) = nub x `intersect` y `intersect` z

toElfGroups' :: [String] -> [(String, String, String)]
toElfGroups' [] = []
toElfGroups' (x : y : z : xs) = (x, y, z) : toElfGroups' xs

toElfGroups :: String -> [(String, String, String)]
toElfGroups = toElfGroups' . lines

part2 :: String -> Int
part2 = sum . map (allItemsToPriority . itemsInAllElves) . toElfGroups

main = do
  contents <- readFile "input.txt"
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)