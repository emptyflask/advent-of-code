{-# LANGUAGE RecordWildCards #-}

import           Data.List          (foldl')
import           Text.Parsec
import           Text.Parsec.String

main :: IO ()
main = do
  let example = [Forward 10, Move North 3, Forward 7, Turn 90, Forward 11]
  putStrLn $ "Example 1: " <> (show . solve1 $ example)
  putStrLn $ "Example 2: " <> (show . solve2 $ example)

  instructions <- parseFromFile parseInstructions "./12/input.txt"

  case instructions of
    Left err -> print err
    Right input -> do
      putStrLn $ "Part 1: "    <> (show . solve1 $ input)
      putStrLn $ "Part 2: "    <> (show . solve2 $ input)

data Dir = North | South | East | West deriving (Show)
type Pos = (Int, Int)
data Ship = Ship {dir :: Dir, pos :: Pos, target :: Pos} deriving (Show)
data Instruction = Move Dir Int | Turn Int | Forward Int deriving (Show)

solve1 :: [Instruction] -> Int
solve1 = manhattanDistance . pos . run
  where
    run = foldl' update start
    start = Ship {dir = East, pos = (0,0), target = (0,0)}

    update :: Ship -> Instruction -> Ship
    update s@Ship {..} (Move d n)  = s {pos = move pos d n}
    update s@Ship {..} (Turn n)    = s {dir = turn dir n}
    update s@Ship {..} (Forward n) = update s (Move dir n)

solve2 :: [Instruction] -> Int
solve2 = manhattanDistance . pos . run
  where
    run = foldl' update' start
    start = Ship {dir = East, pos = (0,0), target = (10,1)}

    update' :: Ship -> Instruction -> Ship
    update' s@Ship {..} (Move d n)  = s {target = move target d n}
    update' s@Ship {..} (Turn n)    = s {target = rotate (normalize n) target}
    update' s@Ship {..} (Forward n) = s {pos = mulOffset n pos target }

rotate :: Int -> Pos -> Pos
rotate 90  (x,y) = ( y, -x)
rotate 180 (x,y) = (-x, -y)
rotate 270 (x,y) = (-y,  x)
rotate n _       = error ("this ship can't turn " <> show n <> " degrees")

mulOffset :: Int -> Pos -> Pos -> Pos
mulOffset i (x,y) (m,n) = (x+m*i, y+n*i)

manhattanDistance :: Pos -> Int
manhattanDistance (x,y) = abs x + abs y

move :: Pos -> Dir -> Int -> Pos
move (x,y) North n = (x, y+n)
move (x,y) South n = (x, y-n)
move (x,y) East n  = (x+n, y)
move (x,y) West n  = (x-n, y)

turn :: Dir -> Int -> Dir
turn dir n = degToDir . normalize $ dirToDeg dir + n

dirToDeg :: Dir -> Int
dirToDeg East  = 0
dirToDeg South = 90
dirToDeg West  = 180
dirToDeg North = 270

degToDir :: Int -> Dir
degToDir 0   = East
degToDir 90  = South
degToDir 180 = West
degToDir 270 = North

normalize :: Integral a => a -> a
normalize n = n `div` 90 * 90 `mod` 360

-- parsing --

parseInstructions :: Parser [Instruction]
parseInstructions = pInstruction `sepEndBy` char '\n'

pInstruction :: Parser Instruction
pInstruction = choice [ Move <$> pDirection <*> pInt
                      , Turn <$> pRotation
                      , Forward <$ char 'F' <*> pInt
                      ]

pDirection :: Parser Dir
pDirection = choice [ North <$ char 'N'
                    , South <$ char 'S'
                    , East  <$ char 'E'
                    , West  <$ char 'W'
                    ]

pRotation :: Parser Int
pRotation = do
  d <- choice [  1 <$ char 'R'
              , -1 <$ char 'L'
              ]
  n <- pInt
  return (d * n)

pInt :: Parser Int
pInt = read <$> many1 digit
