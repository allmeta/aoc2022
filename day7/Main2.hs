import Debug.Trace
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M


main :: IO ()
main = 
  interact (
    show .
    solve
  )

-- total disk space 70000000
-- unused needed: 30000000
-- find total, and find smallest big enough
solve xs = minimum $ filter (>=needed) $ map snd tree
  where 
    dirs = findSizes "" [] $ lines xs
    names = map fst dirs
    tree = M.toList $ updateParents names $ M.fromList $ dirs
    used = traceShowId $ fromJust $ lookup "/" tree
    closest = traceShowId $ filter ((=="/") . parentPath . fst) tree
    total = 70000000
    m = 30000000
    needed = traceShowId $ m - (total - used)

findSizes :: String -> [(String,Int)] -> [String] -> [(String,Int)]
findSizes _ dirs [] = dirs
findSizes path dirs (x:xs) =
  case words x !! 1 of
  "cd" -> case words x !! 2 of
          ".." -> findSizes (parentPath path) dirs xs
          x    -> findSizes (newPath path (last $ words x)) dirs xs
  "ls" -> findSizes path newList newStart
  where 
    (ls,newStart) = span (\l-> head l /= '$') xs
    size = sum . map read . filter (/= "dir") . map (head . words) $ ls
    newList = (path,size):dirs

-- should not need to recurse here actually, updating parent might be enough

updateParents :: [String] -> Map String Int -> Map String Int
updateParents [] dirs = dirs
updateParents (n:ns) dirs = updateParents ns $ M.adjust (+size) p dirs
  where
    size = dirs M.! n
    p = parentPath n

newPath "" "/" = "/"
newPath "/" x = "/"++x
newPath y x = y++"/"++x

parentPath p = case splitOn "/" p of
               ["",""] -> ""
               ["",x] -> "/"
               x -> intercalate "/" $ init x
