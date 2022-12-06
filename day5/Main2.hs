import Data.List.Split
import Data.List
import qualified Data.IntMap.Strict as I
import Debug.Trace (traceShowId)

main :: IO()
main = 
  interact(
    solve .
    splitOn "\n\n"
  )

solve [stacks, instructions] = map (head . snd) $ sortOn fst $ I.toList $ foldl move s $ lines instructions
  where s = I.fromList $ zip [1..] $ map (head . words . init . head) $ chunksOf 4 $ tail $ transpose $ lines stacks

move s ins = I.insert to newz $ I.insert from newy s
  where [mv,from,to] = map (read . last) $ chunksOf 2 $ words ins :: [Int] -- [move x, from y, to z]
        (oldy, newy) = splitAt (mv) $ s I.! from
        newz = oldy ++ (s I.! to)
