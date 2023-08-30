import Data.List (break, nub)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Set qualified as Set
import Debug.Trace (trace)

createParentsAndSizes :: String -> [String] -> (Map.Map String String, Map.Map String Int) -> (Map.Map String String, Map.Map String Int)
createParentsAndSizes _ [] (parents, sizes) =
  (parents, sizes)
createParentsAndSizes folder ("$ cd .." : commands) (parents, sizes) =
  createParentsAndSizes (fromMaybe "/" $ Map.lookup folder parents) commands (parents, sizes)
createParentsAndSizes folder (('$' : ' ' : 'c' : 'd' : ' ' : targetFolder) : commands) (parents, sizes) =
  createParentsAndSizes targetFolder commands (parents, sizes)
createParentsAndSizes folder ("$ ls" : commands) (parents, sizes) =
  createParentsAndSizes folder commands (parents, sizes)
createParentsAndSizes folder (('d' : 'i' : 'r' : ' ' : childFolderName) : commands) (parents, sizes) =
  createParentsAndSizes folder commands (Map.insert childFolderName folder parents, Map.insert childFolderName 0 sizes)
createParentsAndSizes folder (fileInfo : commands) (parents, sizes) =
  createParentsAndSizes folder commands (newParents, newSizes)
  where
    (fileSizeAsString, ' ' : fileName) = break (== ' ') fileInfo
    fileSize = read fileSizeAsString :: Int
    newParents = Map.insert fileName folder parents
    newSizes = Map.insert fileName fileSize sizes

updateSizes' :: [String] -> Map.Map String String -> Map.Map String Int -> Map.Map String Int
updateSizes' [] parents sizes = sizes
updateSizes' children parents sizes | trace (show children) False = undefined
updateSizes' children parents sizes = updateSizes' (nub $ map fst listToAddParents) parents (mergeSizes listToAddParents sizes)
  where
    sizesOfChildren = mapMaybe (\x -> (,) x <$> Map.lookup x sizes) children
    listToAddParents = mapMaybe (\(child, size) -> (,) <$> Map.lookup child parents <*> pure size) sizesOfChildren
    mergeSizes [] sizes = sizes
    mergeSizes ((n, s) : xs) sizes = mergeSizes xs (Map.insertWith (+) n s sizes)

updateSizes :: (Map.Map String String, Map.Map String Int) -> Map.Map String Int
updateSizes (parents, sizes) = updateSizes' (Map.keys (Map.filter (> 0) sizes)) parents sizes

constructFileSystem commands = (parents, updateSizes (parents, sizes), folders)
  where
    (parents, sizes) = createParentsAndSizes "/" commands (Map.empty, Map.empty)
    folders = Map.keys . Map.filter (== 0) $ sizes

part1 contents = sum . filter (< 100000) . mapMaybe (`Map.lookup` sizes) $ folders
  where
    (parents, sizes, folders) = constructFileSystem (lines contents)

main = do
  contents <- readFile "input.txt"
  print (part1 contents)