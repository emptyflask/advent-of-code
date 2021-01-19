import           Control.Monad (guard)
import           Data.List     (group, sort)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Vector   ((!), (!?))
import qualified Data.Vector   as V

main :: IO ()
main = do
  example <- readFile "11/example.txt"
  input <- readFile "11/input.txt"

  putStrLn $ "Example 1: " <> (show . solve1 $ example)
  putStrLn $ "Part 1: "    <> (show . solve1 $ input)

  -- putStrLn $ "Example 2: " <> (show . solve2 $ example)
  -- putStrLn $ "Part 2: "    <> (show . solve2 $ input)

data Seat = Empty | Occupied | Floor deriving (Eq, Ord)
newtype Seats = Seats {unSeats :: V.Vector (V.Vector Seat)} deriving (Eq)

instance Show Seat where
  show Empty    = "L"
  show Occupied = "#"
  show Floor    = "."

instance Show Seats where
  show (Seats s) = unlines . V.toList $ V.map (concat . V.map show) s

solve1 :: String -> Int
solve1 = occupied . countAll . final
  where
    final = converge (==) . iterate tick . toSeats . lines
    countAll = count . V.toList . V.concat . V.toList . unSeats


tick :: Seats -> Seats
tick seats = Seats . V.imap (\y row -> V.imap (\x s -> updateSeat s x y) row) $ unSeats seats
  where
    updateSeat s x y
      | s == Empty && occupied c == 0 = Occupied
      | s == Occupied && occupied c >= 4 = Empty
      | otherwise = s
      where
        c = count $ neighbors x y seats

converge :: (a -> a -> Bool) -> [a] -> a
converge p (x:ys@(y:_))
    | p x y     = y
    | otherwise = converge p ys


occupied :: M.Map Seat Int -> Int
occupied = fromMaybe 0 . M.lookup Occupied

-- unoccupied :: M.Map Seat Int -> Int
-- unoccupied = fromMaybe 0 . M.lookup Empty

-- floor :: M.Map Seat Int -> Int
-- floor = fromMaybe 0 . M.lookup Floor


count :: Ord a => [a] -> M.Map a Int
count list = M.fromList [(head x, length x) | x <- group . sort $ list]

neighbors :: Int -> Int -> Seats -> [Seat]
neighbors x y seats = catMaybes $ do
  i <- [-1..1]
  j <- [-1..1]
  guard ((i,j) /= (0,0))
  return $ findSeat (x+i) (y+j) seats

findSeat :: Int -> Int -> Seats -> Maybe Seat
findSeat x y (Seats grid) = (grid !? y) >>= \row -> row !? x


toSeats :: [String] -> Seats
toSeats = Seats . V.fromList . map (V.fromList . map parseSeat)

parseSeat :: Char -> Seat
parseSeat 'L' = Empty
parseSeat '#' = Occupied
parseSeat '.' = Floor
