import Data.List.Split
import Debug.Trace
import Data.Foldable (traverse_)

main :: IO ()
main = do
  file <- getContents
  traverse_ putStrLn . chunksOf 40 . solve2 . zip [0..] . reverse . solve [1] . map words . lines $ file

solve xs [] = xs
solve (x:xs) (i:is) =
  case head i of
  "noop" -> solve (x:x:xs) is
  "addx" -> solve (((+x) $ read $ last i):x:x:xs) is

solve2 [] = ""
solve2 ((i,x):xs) = lit : solve2 xs
  where ye = j `elem` [v-1,v,v+1]
        lit = if ye then '#' else '.'
        v = x `mod` 40
        j = i `mod` 40
