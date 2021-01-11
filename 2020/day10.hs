import           Control.Applicative (liftA2)
import           Data.IntMap.Strict  (IntMap, fromList, (!))
import           Data.List           (group, sort)

main :: IO ()
main = do
  example <- readLines "10/example.txt"
  input   <- readLines "10/input.txt"

  putStrLn $ "Example 1: " <> (show . solve1 $ example)
  putStrLn $ "Part 1: "    <> (show . solve1 $ input)

  putStrLn $ "Example 2: " <> (show . solve2 $ example)
  putStrLn $ "Part 2: "    <> (show . solve2 $ input)


readLines :: Read b => FilePath -> IO [b]
readLines f = map read . lines <$> readFile f

jolts :: [Int] -> [Int]
jolts as = sort $ 0:(3 + maximum as):as


-- What is the number of 1-jolt differences multiplied by the number of 3-jolt
-- differences?

solve1 :: [Int] -> Int
solve1 as = answer . count $ differences
  where
    answer :: IntMap Int -> Int
    answer = \a -> (a ! 1) * (a ! 3)

    count :: [Int] -> IntMap Int
    count = fromList . map (liftA2 (,) head length) . group . sort

    differences = zipWith (-) (tail $ jolts as) (jolts as)


-- What is the total number of distinct ways you can arrange the adapters to
-- connect the charging outlet to your device?

solve2 :: [Int] -> Int
solve2 as = undefined
