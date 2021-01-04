import           Control.Arrow       (second)
import           Control.Monad.State
import           Data.Either         (rights)
import           Data.Vector         (Vector, fromList, (!?), (//))

data Instruction = NOP Int | ACC Int | JMP Int deriving (Show)
type Result = Either CpuState Int
type CpuState = (Log, Int)
type Log = [(Int, Instruction)]

main :: IO ()
main = do
  input <- readFile "./08/input.txt"
  let instructions = parse input
  putStrLn $ "Part 1: " <> show (solve1 instructions)
  putStrLn $ "Part 2: " <> show (solve2 instructions)

solve1 :: Vector Instruction -> Int
solve1 ops = case eval ops of
               Right i     -> i
               Left (_, i) -> i

solve2 :: Vector Instruction -> Int
solve2 ops = case eval ops of
               Right i       -> i
               Left (log, _) -> retry log
                 where
                   retry = head . rights . results
                   results = map (eval . (\c -> ops // [c])) . candidates
                   candidates = map (second flipJmpNop) . filter (isJmpNop . snd)

isJmpNop :: Instruction -> Bool
isJmpNop (JMP _) = True
isJmpNop (NOP _) = True
isJmpNop _       = False

flipJmpNop :: Instruction -> Instruction
flipJmpNop (NOP i) = JMP i
flipJmpNop (JMP i) = NOP i
flipJmpNop other   = other


eval :: Vector Instruction -> Result
eval ops = evalState (execute 0 ops) initialState

initialState :: CpuState
initialState = ([], 0)

execute :: Int -> Vector Instruction -> State CpuState Result
execute i ops = do
  (log, val) <- get
  if elem i . map fst $ log
     then return $ Left (log, val)
     else case ops !? i of
            Just op@(NOP _) -> put ((i,op):log, val)      >> execute (i+1) ops
            Just op@(ACC n) -> put ((i,op):log, val + n)  >> execute (i+1) ops
            Just op@(JMP n) -> put ((i,op):log, val)      >> execute (i+n) ops
            Nothing         -> return $ Right val


parse :: String -> Vector Instruction
parse = fromList . map (toInstruction . words) . lines
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
