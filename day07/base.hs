import Data.List (break, sort)
import Data.Bits (toIntegralSized)

type Name = String
type Size = Int
data FSItem = File Name Size | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsTop :: FSZipper -> FSZipper
fsTop (item, []) = (item, [])
fsTop zipper = fsTop . fsUp $ zipper

fsUp :: FSZipper -> FSZipper
fsUp (item, []) = (item, [])
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
    let (ls, item:rs) = break (nameIs name) items
    in  (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsChildFolder :: Name -> FSItem -> FSItem
fsChildFolder name (Folder folderName folderItems) = Folder folderName (Folder name [] : folderItems)

fsChildFile :: String -> FSItem -> FSItem
fsChildFile str (Folder folderName folderItems) = Folder folderName (File name size : folderItems)
  where (sizeStr, nameStr) = break (==' ') str
        name = nameStr
        size = read sizeStr :: Int

parse :: [String] -> FSZipper -> FSZipper
parse [] zipper = zipper
parse ("$ cd /":operations) _ = parse operations (Folder "/" [], [])
parse ("$ ls": operations) zipper = parse operations zipper
parse ("$ cd .." : operations) zipper = parse operations (fsUp zipper)
parse (('$' : ' ' : 'c' : 'd' : ' ' : folder) : operations) zipper = parse operations (fsTo folder zipper)
parse (('d' : 'i' : 'r' : ' ' : folder) : operations) (currentFolder, crumbs) = parse operations (fsChildFolder folder currentFolder, crumbs)
parse (fileInfo : operations) (currentFolder, crumbs) = parse operations (fsChildFile fileInfo currentFolder, crumbs)

getSize :: (Int->Bool) -> FSItem -> (Int, [Int])
getSize comprarator (Folder name children) = (value, srr)
  where (value, descendants) = foldr ((\(sacc, lacc) (s, l) -> (sacc+s, l++lacc )) . getSize comprarator) (0, []) children
        doInclude = comprarator value
        srr = if doInclude then value:descendants else descendants
getSize _ (File name size)  = (size, [])

part1 :: FSItem -> Int
part1 tree = sum . snd $ getSize (<100000) tree 

part2 :: FSItem -> Int
part2 tree = head . dropWhile (\size -> 70000000 - totalSize + size <= 30000000) . sort $ folders
  where (totalSize, folders) = getSize (const True) tree

main = do
  contents <- readFile "input.txt"
  let tree = fst . fsTop $ parse (lines contents) (undefined,[])
  print $ "part1: " ++ show (part1 tree)
  print $ "part2: " ++ show (part2 tree)