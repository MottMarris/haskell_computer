module Interpreter
(
    AExp(..),
    BExp(..),
    Com (..),
    aval,
    bval,
    eval
) where

import Data.Map
import Machine

--TODO Task 2.1
data AExp =
     N Int
   | V String
   | Plus AExp AExp
     deriving (Eq, Read, Show)

--TODO Task 2.2

aval :: AExp -> State -> Val

justToInt (Just x) = x

aval (N x) s = x
aval (V v) s = justToInt (Data.Map.lookup v s)
aval (Plus a1 a2) s = (aval a1 s) + (aval a2 s)

--TODO Task 2.1
data BExp =
      Bc Bool
    | Not BExp
    | And BExp BExp
    | Less AExp AExp
      deriving (Eq, Read, Show)

--TODO Task 2.3
bval :: BExp -> State -> Bool

bval (Bc b) s = b
bval (Not b) s = not (bval b s)
bval (And b1 b2) s = (bval b1 s) && (bval b2 s)
bval (Less a1 a2) s = (aval a1 s) < (aval a2 s)

--TODO Task 2.1
data Com =
    Assign String AExp
    | Seq Com Com
    | If BExp Com Com
    | While BExp Com
    | SKIP
    deriving (Eq, Read, Show)

--TODO Task 2.4
eval :: Com -> State -> State

eval (Assign v x) s = Data.Map.insert v (aval x s) s
eval (Seq c1 c2) s = eval c2 (eval c1 s)

eval (If b c1 c2) s | (bval b s) = eval c1 s
                    | otherwise = eval c2 s

eval (While b c) s | (bval b s) = eval (While b c) (eval c s)
                   | otherwise = s

eval SKIP s = s
