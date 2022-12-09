import Data.List
import Debug.Trace (traceShowId)

main :: IO()
main = 
  interact (
    show .
    length .
    nub .
    map snd .
    foldl' solve [((0,0),(0,0))] .
    map (\[i,n]-> (head i, read n)) .
    map words . 
    lines
  )

solve :: [((Int,Int),(Int,Int))] -> (Char,Int) -> [((Int,Int),(Int,Int))]
solve p (d,0) = p
solve (p:pp) (d,n) = solve (np:p:pp) (d,n-1)
  where np = move p $ ds d

move (h@(hx,hy),t@(tx,ty)) (x,y) = (nh,nt)
  where
    nh = (hx+x,hy+y)
    nt = if mdist nh t then h else t

  
mdist (hx,hy) (tx,ty) = abs (hx-tx) > 1 || abs (hy-ty) > 1

ds 'U' = (0,1)
ds 'D' = (0,-1)
ds 'R' = (1,0)
ds 'L' = (-1,0)
