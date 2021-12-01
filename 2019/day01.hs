main :: IO ()
main = do
  input <- readFile "./01/input.txt"
  let list = read <$> lines input
  print $ "Part 1: " <> show (solve1 list)
  print $ "Part 2: " <> show (solve2 list)

solve1 = sum . map fuelForMass
solve2 = sum . map fuelForMassAndFuel

fuelForMass :: (Integral a) => a -> a
fuelForMass n = (n `div` 3) - 2

fuelForMassAndFuel :: (Integral a) => a -> a
fuelForMassAndFuel = sum . takeWhile (> 0) . drop 1 . iterate fuelForMass
