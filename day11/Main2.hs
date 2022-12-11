import Data.List.Split
import Data.List
import Debug.Trace (traceShowId)

main :: IO()
main = do
  file <- getContents
  let monkeys = map parse $ map lines $ splitOn "\n\n" file
  let rounds = 10000 
  let sims = filter (not . null) $ simulate rounds 0 [[[]]] monkeys
  -- putStrLn $ show $ product $ take 2 $ reverse $ sort $ map length $ concatMap transpose $ traceShowId sims
  putStrLn $ show $ product $ take 2 $ reverse $ sort $ map sum $ transpose $ map (map length) sims


parse :: [String] -> (Int,[Int],(Int -> Int), Int, Int, Int)
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
simulate :: Int -> Int -> [[[Int]]] -> [(Int,[Int],(Int -> Int), Int, Int, Int)] -> [[[Int]]]
simulate 0 _ inspections _ = inspections -- return total number of inspections
simulate round turn (ins:inspections) ms
  | turn == length ms = simulate (round-1) 0 ([]:ins:inspections) ms -- new round
  | null startingItems' = simulate round (turn+1) (ni:inspections) ms -- skip monkeys with no items
  | otherwise = simulate round (turn+1) (ni:inspections) nms
  where 
    (monkey',startingItems',operation',test',ifTrue',ifFalse') = ms !! turn
    ni = startingItems':ins
    newItems = map (throw . operation') $ startingItems'
    throw o = if o `mod` test' == 0 then (o, ifTrue') else (o, ifFalse')
    nms = remove turn $ foldl update ms newItems -- throw items to new monkeys, return new monkey state

update ms (item,nm) = before ++ ((monkey',items++[item],operation',test',ifTrue',ifFalse'):after) -- remove own items
  where (before,(m@(monkey',items,operation',test',ifTrue',ifFalse'):after)) = splitAt nm ms

remove turn ms = before ++ ((monkey',[],operation',test',ifTrue',ifFalse'):after)
  where (before, (m@(monkey',_,operation',test',ifTrue',ifFalse'):after)) = splitAt turn ms

op "*" = (*)
op "+" = (+)
op "-" = (-)
