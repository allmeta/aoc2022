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
  where np = move p $ ds d

move (h@(hx,hy):ts) (x,y) = update (nh:ts)
  where
    nh = (hx+x,hy+y)

update [] = []
update [t] = [t]
update (h:t:ts) = h : nt
  where 
    nt = if mdist h t then update $ updateTail h t:ts else t:ts

mdist (hx,hy) (tx,ty) = abs (hx-tx) > 1 || abs (hy-ty) > 1
  
updateTail (hx,hy) (tx,ty)
  | hx/=tx && hy/=ty = (tx+dirx,ty+diry)
  | hx/=tx = (tx+dirx,ty)
  | hy/=ty = (tx,ty+diry)
  | otherwise = (tx,ty)
  where 
    dirx = signum (hx-tx)
    diry = signum (hy-ty)

ds 'U' = (0,1)
ds 'D' = (0,-1)
ds 'R' = (1,0)
ds 'L' = (-1,0)
