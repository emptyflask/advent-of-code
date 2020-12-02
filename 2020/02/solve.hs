import           Control.Applicative
import           Data.Either         (rights)
import           Text.Parsec

data Record = Record { reqs :: [Requirement], pass :: String } deriving Show
data Requirement = Requirement Range Char deriving Show
type Range = (Int, Int)

type Parser a = Parsec String () a

main :: IO ()
main = do
  input <- readFile "input.txt"
  let parsed = parse record "" <$> lines input
  let records = validate <$> rights parsed
  print $ "Part 1: " <> show (length . rights $ records)

validate :: Record -> Either String Record
validate rec@Record{reqs=rs, pass=p} =
  if all (isValid p) rs then Right rec else Left "invalid"

isValid :: String -> Requirement -> Bool
isValid s (Requirement r c) = inRange r $ length matching
  where
    matching = filter (== c) s
    inRange (f,l) n = f <= n && n <= l

record :: Parser Record
record = do
  req <- requirement
  char ':'
  space
  pass <- many1 alphaNum
  return $ Record [req] pass

requirement :: Parser Requirement
requirement = do
  r <- range
  space
  Requirement r <$> alphaNum

range :: Parser Range
range = do
  first <- many1 digit
  char '-'
  last <- many1 digit
  return (read first, read last)
