import           Data.List (tails)

main :: IO ()
main = do
  input <- readFile "./01/input.txt"
  let list = read <$> lines input
  print $ "Part 1: " <> show (solve1 list)
  print $ "Part 2: " <> show (solve2 list)

solve1 :: [Int] -> Int
solve1 = length . filter (uncurry (<)) . pairs

solve2 :: [Int] -> Int
solve2 input = length . filter (uncurry (<)) $ pairs (map sum . windows 3 $ input)

pairs :: [a] -> [(a,a)]
pairs xs@(_:xs') = zip xs xs'

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails
