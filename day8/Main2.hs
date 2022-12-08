import Data.List
import Data.List.Split
import Debug.Trace (traceShowId)

main :: IO ()
main =
  interact
    ( 
    -- for each tallest tree from part1
    -- go through horizontal and vertical, and find total viewing distance
      show
        . solve
        . zipWith (\y l-> zipWith (\x t -> (read [t],(x,y))) [0..] l) [0..]
        . lines
    )

-- (tree, (x,y))
solve :: [[(Int, (Int,Int))]] -> Int
-- could also return viewingDistance of the most occurring trees in every dir
solve xs = maximum $ map (viewingDistance xs)  tallestTrees
 where
  left = init $ tail xs -- trim edges
  right = map reverse left
  top = init $ tail $ transpose xs
  bottom = map reverse top
  tallestTrees = concatMap (concatMap countInit) [left, right, top, bottom]

count :: [(Int,(Int,Int))] -> Int -> [(Int, (Int,Int))] -> [(Int,(Int,Int))]
count trees _ [] = trees
count trees tallest ((t,xy) : ts) = if t > tallest then count ((t,xy):trees) t ts else count trees tallest ts


-- setting tallest as edge, and skipping it
countInit ls = count [] (-1) ls

viewingDistance :: [[(Int,(Int,Int))]] -> (Int,(Int,Int)) -> Int
viewingDistance xs t@(th,(x,y)) = left * right * top * bottom
  where h = xs !! y
        v = transpose xs !! x
        left = case filter (\(t2,_)-> t2 >= th) $ reverse $ takeWhile (/=t) h of
            [] -> x
            ((_,(x2,_)):_) -> abs(x - x2)
        right = case filter (\(t2,_)-> t2 >= th) $ reverse $ takeWhile (/=t) $ reverse h of
            [] -> length h - x - 1
            ((_,(x2,_)):_) -> abs (x - x2)
        top = case filter (\(t2,_)-> t2 >= th) $ reverse $ takeWhile (/=t) v of
            [] -> y
            ((_,(_,y2)):_) -> abs (y - y2)
        bottom = case filter (\(t2,_)-> t2 >= th) $ reverse $ takeWhile (/=t) $ reverse $ v of
            [] -> length v - y - 1
            ((_,(_,y2)):_) -> abs (y - y2)

        -- for every direction
        -- trees = takeWhile (/=t)
        -- reverse, count distance to >= tree
        -- multiply these 4 directions
