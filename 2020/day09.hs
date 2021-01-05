import           Data.List

main :: IO ()
main = do
  example <- readFile "09/example.txt"
  let exlist = map read . lines $ example
      ex1 = solve1 exlist 5
      ex2 = ex1 >>= solve2 exlist
  putStrLn $ "Example 1: " <> show ex1
  putStrLn $ "Example 2: " <> show ex2

  input <- readFile "09/input.txt"
  let list = map read . lines $ input
      part1 = solve1 list 25
      part2 = part1 >>= solve2 list
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2

solve1 :: [Int] -> Int -> Maybe Int
solve1 xs size = fmap (head . snd) $
  find (\(w,n) -> head n `notElem` [ x + y | x <- w, y <- w, x /= y]) $
    splitAt size <$> windows (size+1) xs

solve2 :: [Int] -> Int -> Maybe Int
solve2 list n = uncurry (+) . minmax <$> contiguous
  where
    contiguous = find (\a -> sum a == n) $
      concatMap (`windows` list) [2..(length list)]

    minmax xs = (minimum xs, maximum xs)

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails
