import Data.List

main :: IO()
main = 
  interact (
    show .
    length .
    nub .
    map snd .
    scanl solve ((0,0),(0,0)) .
    map (\[i,n]-> (head i, read n)) .
    map words . 
    lines
  )

solve :: ((Int,Int),(Int,Int)) -> (Char,Int) -> ((Int,Int),(Int,Int))
solve p (d,0) = p
solve p (d,n) = solve np (d,n-1)
  where np = move p $ ds d

move (h@(hx,hy),t@(tx,ty)) (x,y) = (nh,nt)
  where
    nh = (hx+x,hy+y)
    nt = if mdist nh t > 1 then h else t

  
mdist (hx,hy) (tx,ty) = abs (tx-hx) + abs (ty-hy)

ds 'U' = (0,1)
ds 'D' = (0,-1)
ds 'R' = (1,0)
ds 'L' = (-1,0)
