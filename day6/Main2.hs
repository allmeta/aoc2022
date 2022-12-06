import Data.List

main :: IO ()
main = 
  interact (
    show .
    solve 0
  )

solve i xs
  | (nub $ take 14 xs) == take 14 xs = i + 14
  | otherwise = solve (i+1) (tail xs)
