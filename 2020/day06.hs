import           Data.List (groupBy)
import qualified Data.Set  as S

main :: IO ()
main = do
  input <- readFile "./06/input.txt"
  putStrLn $ "Part 1: " <> show (solve1 input)
  putStrLn $ "Part 2: " <> show (solve2 input)

solve1 :: String -> Int
solve1 input = sum $ map (S.size . S.fromList) $ blocks input

solve2 :: String -> Int
solve2 input = sum . count $ blocks' input
    where
      count = map ((S.size . intersect) . map S.fromList)
      intersect = foldr S.intersection (S.fromList['a'..'z'])

blocks :: String -> [String]
blocks = map concat . groupBy (const (not . null)) . lines

blocks' :: String -> [[String]]
blocks' = map (filter (not . null)) . groupBy (const (not . null)) . lines
