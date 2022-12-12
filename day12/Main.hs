import Data.Char
import Data.Maybe
import Data.List
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace (traceShowId)

main :: IO ()
main = do
  file <- getContents
  let (vertices,edges) = mkGrid file
  let start = fromJust $ V.elemIndex 'S' $ vertices
  -- let start = head $ filter (\(s,_,_)->s=='S') $ concat grid
  let minPath = findPaths ('S',start) 0 (maxBound :: Int) (V.replicate (length vertices) False) (vertices,edges)
  print minPath
  -- map grid
  -- make adjacency list with vertices and edges in separate shit
  -- both vectors
  -- make visitedNodes a vector
mkGrid :: String -> (Vector Char, Vector [Int])
mkGrid xs = (vertices, edges)
  where 
    grid = zipWith (\y l-> zipWith (\x t -> (t,x,y)) [0..] l) [0..] $ lines xs
    vertices = V.fromList $ map (\(t,_,_)->t) $ concat grid
    edges = V.fromList $ map (\n-> findNeighbors n grid) $ concat grid
    w = length $ head $ grid

-- find allowed neighbors
findNeighbors (v,x,y) grid = map (\(_,x,y)->y*w+x) $ filter (\(w,_,_)->isAllowed v w) $ catMaybes [left,right,up,down]
  where
    w = length $ head grid
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
isAllowed v w = (abs (ord w - ord v)) <= 1

findPaths (v,i) len minPath visited (vertices,edges)
  | v == 'E' = min len minPath
  | otherwise = foldl' (\l n->findPaths n (len+1) l nv (vertices,edges)) minPath neighbors
  where 
    nv = V.update visited $ V.fromList [(i,True)] -- update visited nodes
    neighbors = map (\n-> (vertices V.! n,n)) $ filter (\n-> not $ visited V.! n) $ edges V.! i


(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0     = Nothing
             -- Definition adapted from GHC.List
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k-1)) (const Nothing) xs n
