import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  input <- readFile "./01/input.txt"
  let list = map (map read) <$> splitWhen (=="") $ lines input

  print $ "Part 1: " <> show (solve1 list)
  print $ "Part 2: " <> show (solve2 list)

solve1 :: [[Int]] -> Int
solve1 = maximum . map sum

solve2 :: [[Int]] -> Int
solve2 = sum . take 3 . reverse . sort . map sum
