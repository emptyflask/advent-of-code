import           Control.Applicative (liftA2)
import           Control.Monad       (guard)
import           Data.Either         (fromRight)
import           Data.Functor        ((<&>))
import           Data.Map            (fromList, (!?))
import           Text.Parsec
import           Text.Parsec.String

type Passports = [Passport]
type Passport = [Pair]
type Pair = (String, String)

main :: IO ()
main = do
  input <- readFile "./04/input.txt"
  let passports = parsePassports input
  putStrLn $ "Part 1: " <> show (solve1 passports)
  putStrLn $ "Part 2: " <> show (solve2 passports)

solve1 :: Passports -> Int
solve1 ps = length $ filter validate1 ps

validate1 :: Passport -> Bool
validate1 ps = all (`elem` keys) required
  where
    keys = map fst ps
    required = ["byr","ecl","eyr","hcl","hgt","iyr","pid"]

solve2 :: Passports -> Int
solve2 ps = length $ filter validate2 ps

validate2 :: Passport -> Bool
validate2 ps = Just True == (foldr1 . liftA2) (&&) [byr,ecl,eyr,hcl,hgt,iyr,pid]
  where
    byr = m !? "byr" <&> inRange 1920 2002 . read
    iyr = m !? "iyr" <&> inRange 2010 2020 . read
    eyr = m !? "eyr" <&> inRange 2020 2030 . read
    hgt = m !? "hgt" <&> validHeight
    hcl = m !? "hcl" <&> isHex
    ecl = m !? "ecl" <&> flip elem ["amb","blu","brn","gry","grn","hzl","oth"]
    pid = m !? "pid" <&> (\s -> length s == 9)
    m = fromList ps

parsePassports :: String -> Passports
parsePassports src = fromRight [] (parse list "" src)

list :: Parser Passports
list = pairs `sepEndBy` ws

pairs :: Parser Passport
pairs = pair `sepEndBy` ws

pair :: Parser Pair
pair = (,) <$> (many1 alphaNum <* char ':') <*> many1 (alphaNum <|> char '#')

ws :: Parser Char
ws = oneOf " \n"

color :: Parser String
color = char '#' *> count 6 hexDigit

height :: Parser String
height = heightImperial <|> heightMetric

heightImperial :: Parser String
heightImperial = do
  val <- try (many1 digit <* string "in")
  guard (inRange 59 76 $ read val) <?> "valid inches"
  return val

heightMetric :: Parser String
heightMetric = do
  val   <- try (many1 digit <* string "cm")
  guard (inRange 150 193 $ read val) <?> "valid cm"
  return val

validHeight :: String -> Bool
validHeight a =
  case parse height "height" a of
    Left _  -> False
    Right _ -> True

isHex :: String -> Bool
isHex a = case parse color "color" a of
            Left _  -> False
            Right _ -> True

inRange :: (Ord a) => a -> a -> a -> Bool
inRange a b n = a <= n && n <= b
