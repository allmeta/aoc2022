import Data.List
import Data.List.Split

main :: IO()
main = 
  interact (
    show .
    sum .
    take 3 .
    reverse .
    sort .
    map (
      sum .
      map read .
      lines
    ) .
    splitOn "\n\n"
  )
