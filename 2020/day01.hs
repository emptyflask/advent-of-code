main :: IO ()
main = do
  input <- readFile "./01/input.txt"
  let list = read <$> lines input
  print $ "Part 1: " <> (show $ solve1 list)
  print $ "Part 2: " <> (show $ solve2 list)

solve1 :: [Int] -> Maybe Int
solve1 xs = mHead $ [ a*b | a <- xs,
                            b <- xs,
                            a + b == 2020 ]

solve2 :: [Int] -> Maybe Int
solve2 xs = mHead $ [ a*b*c | a <- xs,
                              b <- xs,
                              c <- xs,
                              a + b + c == 2020 ]

mHead :: [a] -> Maybe a
mHead (x:_) = Just x
mHead []    = Nothing
