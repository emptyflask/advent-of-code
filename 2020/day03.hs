import           Data.Ratio
import           GHC.Arr

type World = [String]
type Slope = Rational

main :: IO ()
main = do
  input <- readFile "./03/input.txt"
  let w = lines input
  print $ "Part 1: " <> show (solve1 w)
  print $ "Part 2: " <> show (solve2 w)

solve1 :: World -> Int
solve1 = trees (3 % 1)

solve2 :: World -> Int
solve2 w =
  let slopes = [1%1, 3%1, 5%1, 7%1, 1%2]
   in product $ map (`trees` w) slopes

trees :: Slope -> World -> Int
trees s w = snd $ foldl detect (0, 0) rows
    where
      rows = everyNth y w
      detect (xpos, hits) row = (xpos+x, case row !!@ xpos of
                                           '#' ->  hits + 1
                                           _   ->  hits
                                )
      x = fromInteger $ numerator s
      y = fromInteger $ denominator s

everyNth :: Int -> [a] -> [a]
everyNth n xs = map snd $ filter (\(i,_) -> i `mod` n == 0) $ zip [0..] xs

-- circular lookup, faster than `(cycle a) !! i`
(!!@) :: [a] -> Int -> a
(!!@) xs i = xs !! (i `mod` length xs)
