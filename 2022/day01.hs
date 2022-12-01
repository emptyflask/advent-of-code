import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  input <- readFile "./01/input.txt"
  let list = parse input

  print $ "Part 1: " <> show (solve1 list)
  print $ "Part 2: " <> show (solve2 list)

parse :: String -> [[Int]]
parse = map (map read) <$> splitWhen (=="") . lines

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum
