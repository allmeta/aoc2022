import Data.List.Split
import Data.List
import Debug.Trace (traceShowId)

main :: IO()
main = do
  file <- getContents
  let monkeys = map parse $ map lines $ splitOn "\n\n" file
  let rounds = 20
  let sims = simulate rounds 0 (replicate (length monkeys) 0) monkeys
  putStrLn $ show $ take 2 $ reverse $ sort $ sims


parse :: [String] -> (Int,[Integer],(Integer -> Integer), Int, Int, Int)
parse [monkey,startingItems,operation,test,ifTrue,ifFalse] = (monkey',startingItems',operation',test',ifTrue',ifFalse') 
  where
    monkey' = read $ init $ last $ words monkey
    startingItems' = map read $ splitOn "," $ last $ splitOn ":" startingItems 
    operation' = let [_,"=",left,operator,right] = words $ last $ splitOn ":" operation in 
                  case [left,right] of
                  ["old","old"] -> (\old-> (op operator) old old)
                  [left',"old"] -> (\old-> (op operator) (read left') old)
                  ["old",right'] -> (\old-> (op operator)  old (read right'))
    test' = read $ last $ words test
    ifTrue' = read $ last $ words ifTrue
    ifFalse' = read $ last $ words ifFalse

-- on a monkeys turn, also return a list of items inspected
-- to get the total number of inspected items over 20 turns
simulate :: Int -> Int -> [Int] -> [(Int,[Integer],(Integer -> Integer), Int, Int, Int)] -> [Int]
simulate 0 _ inspections _ = inspections -- return total number of inspections
simulate round turn inspections ms
  | turn == length ms = simulate (round-1) 0 inspections ms -- new round
  | null startingItems' = simulate round (turn+1) inspections ms -- skip monkeys with no items
  | otherwise = simulate round (turn+1) ni nms
  where 
    (monkey',startingItems',operation',test',ifTrue',ifFalse') = ms !! turn
    ni = updateInspections turn (length startingItems') inspections
    newItems = map (throw . operation') startingItems' :: [(Integer,Int)]
    throw :: Integer -> (Integer,Int)
    throw o = if o `mod` fromIntegral test' == 0 then (o, ifTrue') else (o, ifFalse') 
    nms = remove turn $ foldl update ms newItems -- throw items to new monkeys, return new monkey state

update :: [(Int,[Integer],(Integer -> Integer), Int, Int, Int)] -> (Integer,Int) -> [(Int,[Integer],(Integer -> Integer), Int, Int, Int)]
update ms (item,nm) = before ++ ((monkey',items++[item],operation',test',ifTrue',ifFalse'):after) -- remove own items
  where (before,(m@(monkey',items,operation',test',ifTrue',ifFalse'):after)) = splitAt nm ms

remove :: Int -> [(Int,[Integer],(Integer -> Integer), Int, Int, Int)] -> [(Int,[Integer],(Integer -> Integer), Int, Int, Int)]
remove turn ms = before ++ ((monkey',[],operation',test',ifTrue',ifFalse'):after)
  where (before, (m@(monkey',_,operation',test',ifTrue',ifFalse'):after)) = splitAt turn ms

updateInspections :: Int -> Int -> [Int] -> [Int]
updateInspections turn itemLength inspections = before ++ ((i+itemLength):after)
  where (before,(i:after)) = splitAt turn inspections

op "*" = (*)
op "+" = (+)
op "-" = (-)
