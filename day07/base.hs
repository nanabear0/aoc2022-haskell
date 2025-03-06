import Data.List (break)

type Name = String
type Data = Int
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

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

getSize :: FSItem -> (Int, [Int])
getSize (Folder name children) = (fr, srr)
  where (fr, sr) = foldr ((\(sacc, lacc) (s, l) -> (sacc+s, l++lacc )) . getSize) (0, []) children
        isSizeEnough = fr < 100000
        srr = if isSizeEnough then fr:sr else sr
getSize (File name size) = (size, [])

tree = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let tree = fst $ fsUp . fsUp . fsUp $ parse ls (undefined,[])
  return tree

part1 :: FSItem -> Int
part1 tree = sum . snd $ getSize tree
main = do
  contents <- readFile "input.txt"
  let ls = lines contents
  let tree = fst $ fsUp . fsUp . fsUp $ parse ls (undefined,[])
  print $ "part1: " ++ show (part1 tree)