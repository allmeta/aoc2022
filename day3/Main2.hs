import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

main :: IO ()
main = 
  interact (
    show .
    sum .
    map solve .
    chunksOf 3 .
    lines
  )

solve [a,b,c] = fromJust $ lookup z ns 
  where (z:_) = filter (`elem` c) $ filter (`elem` b) a

ns = zip (['a'..'z']++['A'..'Z']) [1..]
