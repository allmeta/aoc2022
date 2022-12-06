import Data.List

main :: IO ()
main = 
  interact (
    show .
    solve 0
  )

solve i xs
  | (nub $ take 4 xs) == take 4 xs = i + 4
  | otherwise = solve (i+1) (tail xs)
