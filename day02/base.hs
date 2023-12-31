import System.IO (readFile)

convertToRPCTuple :: String -> [(Int, Int)]
convertToRPCTuple x =
  map (\y -> (fromEnum (head y) - fromEnum 'A', fromEnum (last y) - fromEnum 'X')) $
    lines x

calcMatchScore :: (Int, Int) -> Int
calcMatchScore (x, y) = (y + 1) + z * 3
  where
    z = mod (y - x + 4) 3

predictMatchScore :: (Int, Int) -> Int
predictMatchScore (x, y) = (z + 1) + y * 3
  where
    z = mod (x + y + 2) 3

part1 :: String -> Int
part1 = sum . map calcMatchScore . convertToRPCTuple

part2 :: String -> Int
part2 = sum . map predictMatchScore . convertToRPCTuple

main = do
  contents <- readFile "input.txt"
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)