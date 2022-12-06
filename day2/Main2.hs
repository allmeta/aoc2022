main :: IO ()
main =
  interact
    ( show
        . sum
        . map (solve . mapSecond . words)
        . lines
    )

mapSecond [a, b] = case b of
  "X" -> [a, lose a]
  "Y" -> [a, a]
  "Z" -> [a, win a]

lose "A" = "C"
lose "B" = "A"
lose "C" = "B"

win "A" = "B"
win "B" = "C"
win "C" = "A"

solve [a, b] = outcome + static b
 where
  outcome = case (a, b) of
    ("A", "A") -> 3
    ("B", "B") -> 3
    ("C", "C") -> 3
    ("A", "B") -> 6
    ("B", "C") -> 6
    ("C", "A") -> 6
    ("A", "C") -> 0
    ("B", "A") -> 0
    ("C", "B") -> 0
    _ -> 0

static "X" = 1
static "Y" = 2
static "Z" = 3
static "A" = 1
static "B" = 2
static "C" = 3
