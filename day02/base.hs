import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

convertToRPCTuple :: String -> [(Int, Int)]
convertToRPCTuple x =
  map (\y -> (fromEnum (head y) - fromEnum 'A', fromEnum (last y) - fromEnum 'X')) $
    lines x

calcMatchScore :: (Int, Int) -> Int
calcMatchScore (x, y) = (y + 1) + z * 3
    where z = mod (y - x + 4) 3

predictMatchScore:: (Int, Int) -> Int
predictMatchScore (x, y) =  (z + 1) + y *3
        where z = mod (x + y + 2) 3

part1 :: String -> Int
part1 x = sum $ map calcMatchScore $ convertToRPCTuple x

part2 :: String -> Int
part2 x = sum $ map predictMatchScore $ convertToRPCTuple x

main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  putStrLn $ "part1: " ++ show (part1 contents)
  putStrLn $ "part2: " ++ show (part2 contents)
  hClose handle