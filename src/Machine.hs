module Machine
(
        Vname,
        Val,
        State,
        Instr (..),
        Stack,
        Config,
        iexec,
        exec
) where

import Data.Map

--TODO Task 1.1
type Vname = String
--TODO Task 1.2
type Val = Int
--TODO Task 1.3
type State = Map Vname Val

--TODO Task 1.4
data Instr =
        LOADI Int
      | LOAD Vname
      | ADD
      | STORE Vname
      | JMP Int
      | JMPLESS Int
      | JMPGE Int
        deriving (Eq, Read, Show)

--TODO Task 1.5
type Stack = [Int]

--TODO Task 1.6
type Config = (Val, State, Stack)

--TODO Task 1.7
just2int (Just x) = x
getValue v m = just2int (Data.Map.lookup v m)
add (x:y:xs) = (x + y) : xs
pop (x:xs) = xs
compareTopTwoLess (x:y:xs) = y < x
compareTopTwoGE (x:y:xs) = y >= x

iexec :: Instr -> Config -> Config

iexec (LOADI x) (a, b, c) = (a + 1, b, [x] ++ c)
iexec (LOAD v) (a, b, c) = (a + 1, b, [getValue v b] ++ c)
iexec ADD (a, b, c) = (a + 1, b, (add c))
iexec (STORE v) (a, b, c) = (a + 1, Data.Map.insert v (head c) b, pop c)
iexec (JMP x) (a, b, c) = (a + x + 1, b, c)

iexec (JMPLESS x) (a, b, c) | compareTopTwoLess c = (a + x + 1, b, pop (pop c))
                            | otherwise = (a + 1, b, pop (pop c))

iexec (JMPGE x) (a, b, c) | compareTopTwoGE c = (a + x + 1, b, pop (pop c))
                          | otherwise = (a + 1, b, pop (pop c))



--TODO Task 1.8
exec :: [Instr] -> Config -> Config
exec [] (a, b, c) = (a, b, c)
exec (x:xs) c = exec xs (iexec x c)
