import Data.List (nub)

packetStart' :: Int -> String -> String -> Maybe Int
packetStart' _ "" _ = Nothing
packetStart' t (c : rest) w = if (length . nub) nw == t then Just 1 else succ <$> packetStart' t rest nw
  where
    nw = take t (c : w)

packetStart :: Int -> String -> Maybe Int
packetStart t x = packetStart' t x ""

main = do
  contents <- readFile "input.txt"
  putStrLn $ "part1: " ++ show (packetStart 4 contents)
  putStrLn $ "part2: " ++ show (packetStart 14 contents)