import Data.List
import Data.List.Split

main :: IO ()
main = 
  interact (
    show .
    length .
    filter solve .
    lines
  )

solve x = not $ null $ filter (`elem` s2) s1
  where [a,b] = splitOn "," x
        [a1,a2] = splitOn "-" a
        [b1,b2] = splitOn "-" b
        s1 = [read a1..read a2] :: [Int]
        s2 = [read b1..read b2] :: [Int]
