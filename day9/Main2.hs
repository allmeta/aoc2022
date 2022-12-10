import Data.List
import Debug.Trace (traceShowId)

main :: IO()
main = 
  interact (
    show .
    length .
    nub .
    map last .
    foldl' solve [replicate 10 (0,0)] .
    map (\[i,n]-> (head i, read n)) .
    map words . 
    lines
  )

solve :: [[(Int,Int)]] -> (Char,Int) -> [[(Int,Int)]]
solve p (d,0) = p
solve (p:pp) (d,n) = solve (np:p:pp) (d,n-1)
  where np = traceShowId $ move p $ ds d

move (h@(hx,hy):ts) (x,y) = update h (nh:ts)
  where
    nh = (hx+x,hy+y)

update oldh [] = []
update oldh [t] = [t]
update oldh (h:t:ts) = h : nt
  where 
    nt = if mdist h t then update t (oldh:ts) else t:ts

  
mdist (hx,hy) (tx,ty) = abs (hx-tx) > 1 || abs (hy-ty) > 1

ds 'U' = (0,1)
ds 'D' = (0,-1)
ds 'R' = (1,0)
ds 'L' = (-1,0)
