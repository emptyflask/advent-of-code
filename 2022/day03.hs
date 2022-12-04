import           Data.List       (elemIndex, intersect, splitAt)
import           Data.List.Split (chunksOf)
import           Data.Maybe

main :: IO ()
main = do
  input <- readFile "./03/input.txt"

  print $ "Part 1: " <> show (solve1 input)
  print $ "Part 2: " <> show (solve2 input)

solve1 :: String -> Int
solve1 = sum . map (priority . head . uncurry intersect . split) . lines
  where
    split :: [a] -> ([a],[a])
    split xs = splitAt n xs
      where n = length xs `div` 2

solve2 :: String -> Int
solve2 = sum . map (priority . head . intersect3) . chunksOf 3 . lines
  where
    intersect3 (a:b:c:_) = intersect c $ intersect a b
    intersect3 _         = undefined

priority x = maybe 0 (1 +) (elemIndex x alpha)
  where
    alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
