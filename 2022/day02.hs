import           Data.List

data Action = Rock | Paper | Scissors deriving (Show)
data Outcome = Win | Lose | Draw deriving (Show)

main :: IO ()
main = do
  input <- readFile "./02/input.txt"

  print $ "Part 1: " <> show (solve1 input)
  print $ "Part 2: " <> show (solve2 input)

solve1 :: String -> Int
solve1 = sum . map (uncurry score) . parse
  where
    parse :: String -> [(Action, Action)]
    parse = map ((\(x:y:_) -> (toAction x, toAction y)) . words) . lines
      where
        toAction "A" = Rock
        toAction "B" = Paper
        toAction "C" = Scissors
        toAction "X" = Rock
        toAction "Y" = Paper
        toAction "Z" = Scissors
        toAction _   = error "invalid action"

solve2 :: String -> Int
solve2 = sum . map (\(a,o) -> score a $ findNeededAction a o) . parse
  where
    parse :: String -> [(Action, Outcome)]
    parse = map ((\(x:y:_) -> (toAction x, toOutcome y)) . words) . lines
      where
        toAction "A" = Rock
        toAction "B" = Paper
        toAction "C" = Scissors
        toAction _   = error "invalid action"

        toOutcome "X" = Lose
        toOutcome "Y" = Draw
        toOutcome "Z" = Win
        toOutcome _   = error "invalid outcome"

    findNeededAction :: Action -> Outcome -> Action
    findNeededAction Rock Win      = Paper
    findNeededAction Rock Lose     = Scissors
    findNeededAction Paper Win     = Scissors
    findNeededAction Paper Lose    = Rock
    findNeededAction Scissors Win  = Rock
    findNeededAction Scissors Lose = Paper
    findNeededAction a Draw        = a

score :: Action -> Action -> Int
score a b = scoreAction b + scoreOutcome (play a b)
  where
    play :: Action -> Action -> Outcome
    play Rock Paper     = Win
    play Paper Scissors = Win
    play Scissors Rock  = Win
    play Rock Scissors  = Lose
    play Paper Rock     = Lose
    play Scissors Paper = Lose
    play _ _            = Draw

    scoreAction :: Action -> Int
    scoreAction Rock     = 1
    scoreAction Paper    = 2
    scoreAction Scissors = 3

    scoreOutcome :: Outcome -> Int
    scoreOutcome Lose = 0
    scoreOutcome Draw = 3
    scoreOutcome Win  = 6

