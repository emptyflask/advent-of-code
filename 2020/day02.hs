import           Data.Either (rights)
import           Text.Parsec

data Record = Record { reqs :: [Requirement], pass :: String }
instance Show Record where
  show r = unwords ( show <$> reqs r) ++ ": " ++ pass r

data Requirement = Requirement Range Char
instance Show Requirement where
  show (Requirement (a,b) c) = show a ++ "-" ++ show b ++ " " ++ show c

type Range = (Int, Int)
type Validator = String -> Requirement -> Bool

main :: IO ()
main = do
  input <- readFile "./02/input.txt"
  let records = rights $ parse record "" <$> lines input
  print $ "Part 1: " <> show (solve isValid1 records)
  print $ "Part 2: " <> show (solve isValid2 records)

solve :: Validator -> [Record] -> Int
solve v rs = length . rights $ validate v <$> rs

validate :: Validator -> Record -> Either Record Record
validate validator rec@Record{reqs=rs, pass=p} =
  if all (validator p) rs
     then Right rec
     else Left rec

isValid1 :: Validator
isValid1 s (Requirement r c) = inRange r $ length matching
  where
    matching = filter (== c) s
    inRange (f,l) n = f <= n && n <= l

isValid2 :: Validator
isValid2 s (Requirement (a,b) c) =
  (Just True == first) `xor`
  (Just True == second)
  where
    first  = (== c) <$> s !!? (a-1)
    second = (== c) <$> s !!? (b-1)

-- PARSING --

type Parser a = Parsec String () a

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

-- THINGS THAT SHOULD BE STANDARD --

-- safe version of (!!)
(!!?) :: [a] -> Int -> Maybe a
(!!?) xs n     | n < 0 =  Nothing
(!!?) [] _     =  Nothing
(!!?) (x:_)  0 =  Just x
(!!?) (_:xs) n =  xs !!? (n-1)

xor :: Bool -> Bool -> Bool
x `xor` y = (x || y) && not (x && y)
