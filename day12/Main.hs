import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  file <- getContents
  let grid = mkGrid file
  let start = head $ filter (\(s,_,_)->s=='S') $ concat grid
  let minPath = findPaths start 0 (maxBound :: Int) [] grid
  print minPath
  -- make grid with index
  -- find S and E
  -- A*:
  -- for every adjacent allowed move:
  -- recurse and make multiple paths until we find E
  -- keep track of visited positions so we dont loop
  -- return path length at end
  -- if no path or loop: return Nothing as length
  -- NB: might need to be able to traverse lower as well
  -- maybe its nice to make a tile -> neighbors map for simplicity
  --
mkGrid = zipWith (\y l-> zipWith (\x t -> (t,x,y)) [0..] l) [0..] . lines

findPaths node@(v,x,y) len minPath visited grid
  | v == 'E' = min len minPath
  | otherwise = foldl' (\l n->findPaths n (len+1) l nv grid) minPath neighbors
  where 
    nv = traceShowId $ node:visited
    neighbors = filter (`notElem` visited) $ findNeighbors node grid -- and filter loop

-- find allowed neighbors
findNeighbors (v,x,y) grid = filter (\(w,_,_)->isAllowed v w) $ catMaybes [left,right,up,down]
  where
    left = case grid !? y of
            Nothing -> Nothing
            Just a -> a !? (x-1)
    right = case grid !? y of
            Nothing -> Nothing
            Just a -> a !? (x+1)
    up = case grid !? (y+1) of
            Nothing -> Nothing
            Just a -> a !? x
    down = case grid !? (y-1) of
            Nothing -> Nothing
            Just a -> a !? x

isAllowed 'S' w = w=='a'
isAllowed v 'E' = v=='z'
isAllowed v w = ((ord w) - (ord v)) <= 1

(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
             -- Definition adapted from GHC.List
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
