import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)
import Data.List (intersect, nub)
import Data.Char (isUpper)

-- shared
toPriority :: Char -> Int
toPriority x = fromEnum x - lowerBase
    where lowerBase = if isUpper x then fromEnum 'A' - 27 else fromEnum 'a' - 1

allItemsToPriority :: [Char] -> Int
allItemsToPriority x = sum $ map toPriority x

-- part1
itemsInBothCompartments :: Eq a => ([a], [a]) -> [a]
itemsInBothCompartments (x, y) = nub x `intersect` y

splitToRucksacks :: String -> [([Char], [Char])]
splitToRucksacks x = map (\y -> splitAt (length y `div` 2) y) $ lines x

part1 :: String -> Int
part1 x = sum $ map (allItemsToPriority . itemsInBothCompartments) $ splitToRucksacks x

-- part2
itemsInAllElves :: Eq a => ([a], [a], [a]) -> [a]
itemsInAllElves (x,y,z) = nub x `intersect` y `intersect` z

toElfGroups' :: [String] -> [(String, String, String)]
toElfGroups' [] = []
toElfGroups' (x:y:z:xs) = (x,y,z) : toElfGroups' xs

toElfGroups :: String -> [(String, String, String)]
toElfGroups x = toElfGroups' $ lines x

part2 :: String -> Int
part2 x = sum $ map (allItemsToPriority . itemsInAllElves) $ toElfGroups x


main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)
  hClose handle