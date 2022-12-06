import Data.List
import Data.Maybe
import Debug.Trace

main :: IO ()
main = 
  interact (
    show .
    sum .
    map solve .
    lines
  )

solve x = fromJust $ lookup z ns 
  where l = length x `div` 2
        (a,b) = splitAt l x
        (z:_) = filter (`elem` b) a

ns = zip (['a'..'z']++['A'..'Z']) [1..]
