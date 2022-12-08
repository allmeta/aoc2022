import Data.List
import Data.List.Split
import Debug.Trace (traceShowId)

main :: IO ()
main =
  interact
    ( -- for each row from each angle,
      -- store how many visible trees yet,
      -- and the current tallest tree in the row
      -- then it is easy to sum all the visible trees from everyy angle
      --
      -- change solution to index all trees, and add a tree to a list if it is visible
      -- nub solution and find length
      show
        . solve
        . zipWith (\y l-> zipWith (\x t -> (read [t],(x,y))) [0..] l) [0..]
        . lines
    )

-- (tree, (x,y))
solve :: [[(Int, (Int,Int))]] -> Int
solve ts = (+) edgeSums $ length $ traceShowId $ nub $ concatMap (concatMap countInit) [left, right, top, bottom]
 where
  left = init $ tail $ ts -- trim edges
  right = map reverse left
  top = init $ tail $ transpose ts
  bottom = map reverse top
  edgeSums = 4

count :: [(Int,Int)] -> Int -> [(Int, (Int,Int))] -> [(Int,Int)]
count trees _ [] = trees
count trees tallest ((t,xy) : ts) = if t > tallest then count (xy:trees) t ts else count trees tallest ts


-- setting tallest as edge, and skipping it
countInit ls = traceShowId $ count [] (-1) ls
