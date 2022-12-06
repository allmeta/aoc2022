import Data.List
import Data.List.Split

main :: IO()
main = 
  interact (
    show .
    maximum .
    map (
      sum .
      map read .
      lines
    ) .
    splitOn "\n\n"
  )
