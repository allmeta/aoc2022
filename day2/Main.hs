main :: IO ()
main = 
  interact (
    show .
    solve .
    map words .
    lines
  )

solve [] = 0
solve ([a,b]:xs) = outcome + static b + solve xs
  where outcome = case (a,b) of 
                    ("A","X") -> 3
                    ("B","Y") -> 3
                    ("C","Z") -> 3

                    ("A","Y") -> 6
                    ("B","Z") -> 6
                    ("C","X") -> 6

                    ("A","Z") -> 0
                    ("B","X") -> 0
                    ("C","Y") -> 0
                    _ -> 0

static "X" = 1
static "Y" = 2
static "Z" = 3
