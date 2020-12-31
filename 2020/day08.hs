import           Control.Monad.State

type CpuState = (Log, Int)
data Instruction = NOP Int | ACC Int | JMP Int deriving (Show)
type Log = [Int]

main :: IO ()
main = do
  input <- readFile "./08/input.txt"
  putStrLn $ "Part 1: " <> show (solve1 input)

solve1 :: String -> Int
solve1 str = evalState (execute (parse str) 0) init
  where
    init = ([], 0)

execute :: [Instruction] -> Int -> State CpuState Int
execute ops i = do
  (log, val) <- get
  if i `elem` log
     then return val
     else case ops !! i of
            NOP _ -> put (i:log, val) >> execute ops (i+1)
            ACC n -> put (i:log, val + n) >> execute ops (i+1)
            JMP n -> put (i:log, val) >> execute ops (i+n)

parse :: String -> [Instruction]
parse str = map (toInstruction . words) $ lines str
  where
    toInstruction (op:n:_) = case op of
      "nop" -> NOP $ toNum n
      "acc" -> ACC $ toNum n
      "jmp" -> JMP $ toNum n
      _     -> error "invalid instruction"

    toNum (sym:n) = case sym of
                      '+' -> read n
                      '-' -> negate (read n)
                      _   -> error "invalid value"

example = unlines [ "nop +0"
                  , "acc +1"
                  , "jmp +4"
                  , "acc +3"
                  , "jmp -3"
                  , "acc -99"
                  , "acc +1"
                  , "jmp -4"
                  , "acc +6"
                  ]
