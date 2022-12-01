{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           Data.Char     (digitToInt)
import           Data.Function (on)
import           Data.List     (maximumBy, minimumBy, transpose)
import           Data.Map      (fromListWith, toList)

type Bits = [Bool]

instance {-# OVERLAPPING #-} Show Bits where
  show = concatMap (show . fromEnum) -- display [True, False, False, True] as "1001"

main :: IO ()
main = do
  input   <- readBits <$> readFile "./03/input.txt"
  example <- readBits <$> readFile "./03/example.txt"

  print $ "Example 1: " <> show (solve1 example)
  print $ "Part 1: "    <> show (solve1 input)

  print $ "Example 2: " <> show (solve2 example)
  print $ "Part 2: "    <> show (solve2 input)

solve1 :: [Bits] -> Int
solve1 input = gam * eps
  where
    gam = binToDec $ run maximumBy
    eps = binToDec $ run minimumBy
    run op = (fst . op (compare `on` snd)) . frequency <$> transpose input

-- min/max of each column filters the original list, recurse until one list item remaining
solve2 :: [Bits] -> Int
solve2 input = undefined
  -- where
    -- o2generator = run maximumBy
    -- co2scrubber = run minimumBy
    -- -- run op = (fst . op (compare `on` snd)) . frequency <$> transpose input
    -- run i op input' = filter (\b -> (b !! i) == (head $ run op input)) input'



frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

binToDec :: Bits -> Int
binToDec = foldl (\x y -> fromEnum y + 2 * x) 0

readBits :: String -> [Bits]
readBits = (fmap . fmap) digitToBool . lines
  where
    digitToBool '1' = True
    digitToBool _   = False
