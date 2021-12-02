import           Data.List          (foldl')
import           Text.Parsec
import           Text.Parsec.String

data Command = Forward Int | Down Int | Up Int deriving (Show)
type Pos = (Int, Int)
type PosA = (Int, Int, Int)

main :: IO ()
main = do
  let example = [ Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2 ]
  putStrLn $ "Example 1: " <> (show . solve1 $ example)
  putStrLn $ "Example 2: " <> (show . solve2 $ example)

  cmds <- parseFromFile parseCommands "./02/input.txt"

  case cmds of
    Left err -> print err
    Right input -> do
      putStrLn $ "Part 1: "    <> (show . solve1 $ input)
      putStrLn $ "Part 2: "    <> (show . solve2 $ input)

  where
    parseCommands :: Parser [Command]
    parseCommands = pCmd `sepEndBy` char '\n'

    pCmd :: Parser Command
    pCmd = choice [ Forward <$ string "forward " <*> pInt
                  , Down <$ string "down " <*> pInt
                  , Up <$ string "up " <*> pInt
                  ]

    pInt :: Parser Int
    pInt = read <$> many1 digit


solve1 :: [Command] -> Int
solve1 cs = uncurry (*) dest
  where
    init = (0,0)
    dest = foldl' move init cs

    move :: Pos -> Command -> Pos
    move (x,y) (Forward n ) = (x + n, y)
    move (x,y) (Down n )    = (x, y + n)
    move (x,y) (Up n )      = (x, y - n)


solve2 :: [Command] -> Int
solve2 cs = uncurry (*) $ toPos dest
  where
    init = (0,0,0)
    dest = foldl' moveA init cs

    toPos :: PosA -> Pos
    toPos (x,y,_) = (x,y)

    moveA :: PosA -> Command -> PosA
    moveA (x,y,a) (Forward n ) = (x + n, y + a * n, a)
    moveA (x,y,a) (Down n )    = (x, y, a + n)
    moveA (x,y,a) (Up n )      = (x, y, a - n)
