import           Data.List          (nub)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Text.Parsec
import           Text.Parsec.String

type Bags = M.Map Color Contents
type Color = String
type Contents = [BagCount]
type BagCount = (Int, Color)

main :: IO ()
main = do
  result <- parseFromFile bags "./07/input.txt"
  case result of
    Left err    -> print err
    Right input -> do
      putStrLn $ "Part 1: " <> show (solve1 input)
      putStrLn $ "Part 2: " <> show (solve2 input)


-- How many bag colors can eventually contain at least one shiny gold bag?
solve1 :: Bags -> Int
solve1 bags = length (containersOf "shiny gold") - 1
  where
    containersOf :: String -> [Color]
    containersOf b = nub . concat . takeWhile (not . null) . iterate (>>= search) $ pure b

    search c = fromMaybe [] (M.lookup c bags')
    bags' = invert . removeCount $ bags

    removeCount :: M.Map Color Contents -> M.Map Color [Color]
    removeCount = M.map (map snd)

    invert :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k]
    invert m = M.fromListWith (++) pairs
      where pairs = [(v, [k]) | (k, vs) <- M.toList m, v <- vs]


-- How many individual bags are required inside your single shiny gold bag?
solve2 :: Bags -> Int
solve2 bags = bagCount (search "shiny gold") - 1
  where
    bagCount = foldr (\(n,col) count -> count + n * bagCount (search col)) 1
    search c = fromMaybe [] (M.lookup c bags)


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
  return $ unwords cs
