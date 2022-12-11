import Debug.Trace

main :: IO ()
main = 
  interact (
    show .
    xsum .
    reverse .
    traceShowId .
    solve [1] .
    map words .
    lines
  )

solve xs [] = xs
solve (x:xs) (i:is) =
  case head i of
  "noop" -> solve (x:x:xs) is
  "addx" -> solve (((+x) $ read $ last i):x:x:xs) is


xsum xs = sum $ traceShowId $ map (\n->n*(traceShowId $ xs!!n)) [20,60,100,140,180,220]
