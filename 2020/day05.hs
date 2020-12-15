main :: IO ()
main = do
  input <- readFile "./05/input.txt"
  let seats = seatID <$> lines input
  putStrLn $ "Part 1: " <> show (maximum seats)
  putStrLn $ "Part 2: " <> show (solve2 seats)

solve2 :: [Int] -> Int
solve2 s = (mn+mx) `div` 2 * (length s+1) - sum s
  where
    mn = minimum s
    mx = maximum s

seatID :: String -> Int
seatID s = row * 8 + col
  where
    (r, c) = splitAt 7 s
    row = bisectRow r
    col = bisectCol c

bisectRow :: [Char] -> Int
bisectRow = bisectRow' 0 127
  where
    bisectRow' l _ [] = l
    bisectRow' l h (c:cs) =
      case c of
        'B' -> bisectRow' (h-p) h cs
        'F' -> bisectRow' l (l+p) cs
      where
        p = (h - l) `div` 2

bisectCol :: [Char] -> Int
bisectCol = bisectCol' 0 7
  where
    bisectCol' l _ [] = l
    bisectCol' l h (c:cs) =
      case c of
        'R' -> bisectCol' (h-p) h cs
        'L' -> bisectCol' l (l+p) cs
      where
        p = (h - l) `div` 2
