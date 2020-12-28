import           Data.List          (intercalate)
import qualified Data.Map           as M
import           Text.Parsec
import           Text.Parsec.String

type Bags = M.Map Color Contents
-- type Bags = [(Color, Contents)]
type Color = String
type Contents = [BagCount]
type BagCount = (Int, Color)

main :: IO ()
main = do
  result <- parseFromFile bags "./07/input.txt"
  case result of
    Left err    -> print err
    Right input -> putStrLn $ "Part 1: " <> show input -- (solve1 input)
  -- putStrLn $ "Part 2: " <> show (solve2 input)

solve1 :: Bags -> Int
solve1 input = undefined

bags :: Parser Bags
bags = do
  bs <- bag `sepEndBy` char '\n'
  return $ M.fromList bs

bag :: Parser (Color, Contents)
bag = do
  c <- color
  string "bags contain "
  bs <- contents
  optional (string "no other bags.")
  return (c, bs)

contents :: Parser Contents
contents = bagCount `sepEndBy` (b <> sep)
  where
    b = try (string "bags") <|> string "bag"
    sep = string ", " <|> string "."

bagCount :: Parser BagCount
bagCount = do
  n <- many1 digit
  space
  c <- color
  return (read n, c)

color :: Parser Color
color = do
  cs <- count 2 $ manyTill letter space
  return $ intercalate " " cs
