import Data.Char (digitToInt)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil f (x : xs) = x : if not . f $ x then takeUntil f xs else []

checkVisibility :: [[Int]] -> (Int, Int) -> (Int, Int) -> Bool
checkVisibility trees (xsize, ysize) (myx, myy) = fromTop || fromBottom || fromLeft || fromRight
  where
    smallerThanMe = (< (trees !! myy !! myx))
    applyOp f = all (smallerThanMe . f)
    fromTop = applyOp (\y -> trees !! y !! myx) [0 .. myy - 1]
    fromBottom = applyOp (\y -> trees !! y !! myx) (tail [ysize, ysize - 1 .. myy + 1])
    fromLeft = applyOp (\x -> trees !! myy !! x) [0 .. myx - 1]
    fromRight = applyOp (\x -> trees !! myy !! x) (tail [xsize, xsize - 1 .. myx + 1])

calculateScenicScore :: [[Int]] -> (Int, Int) -> (Int, Int) -> Int
calculateScenicScore trees (xsize, ysize) (myx, myy) = fromTop * fromBottom * fromLeft * fromRight
  where
    condition = (>= (trees !! myy !! myx))
    applyOp f = length . takeUntil (condition . f) . reverse
    fromTop = applyOp (\y -> trees !! y !! myx) [0 .. myy - 1]
    fromBottom = applyOp (\y -> trees !! y !! myx) (tail [ysize, ysize - 1 .. myy + 1])
    fromLeft = applyOp (\x -> trees !! myy !! x) [0 .. myx - 1]
    fromRight = applyOp (\x -> trees !! myy !! x) (tail [xsize, xsize - 1 .. myx + 1])

main = do
  contents <- readFile "input.txt"
  let trees = map (map digitToInt) $ lines contents
  let (xsize, ysize) = (length . head $ trees, length trees)
  let indexes = [(x, y) | y <- [0 .. ysize - 1], x <- [0 .. xsize - 1]]
  print $ "part 1: " ++ (show . length . filter (checkVisibility trees (xsize, ysize)) $ indexes)
  print $ "part 2: " ++ (show . maximum . map (calculateScenicScore trees (xsize, ysize)) $ indexes)
  return ()
