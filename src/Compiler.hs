module Compiler
(
    acomp,
    bcomp,
    ccomp
) where

import Machine
import Interpreter
import Data.Map

--TODO Task 3.1
acomp :: AExp -> [Instr]
acomp (N x) = [LOADI x]
acomp (V v) = [LOAD v]
acomp (Plus a1 a2) = (acomp a1) ++ (acomp a2) ++ [ADD]

--TODO Task 3.2
bcomp :: BExp -> Bool -> Int -> [Instr]
bcomp (Bc b1) b i | b1 == b = [JMP i]
                      | otherwise = []
bcomp (Not b1) b i = bcomp b1 (not b) i
bcomp (And b1 b2) True i = (bcomp b1 False (length(bcomp b2 True i))) ++ (bcomp b2 True i)
bcomp (And b1 b2) False i = (bcomp b1 False (length((bcomp b2 False i))+i)) ++ (bcomp b2 False i)
bcomp (Less a1 a2) True i = (acomp a1) ++ (acomp a2) ++ [JMPLESS i]
bcomp (Less a1 a2) False i = (acomp a1) ++ (acomp a2) ++ [JMPGE i]

--TODO Task 3.3
ccomp :: Com -> [Instr]
ccomp (Assign v a) = (acomp a) ++ [STORE v]
ccomp (Seq c1 c2) = (ccomp c1) ++ (ccomp c2)
ccomp (If b c1 c2) = (bcomp b False (length (ccomp c1) + 1)) ++ (ccomp c1) ++ [JMP (length (ccomp c2))] ++ (ccomp c2)
ccomp (While b c) = (bcomp b False ((length (ccomp c)) + 1)) ++ (ccomp c) ++ [JMP (negate (length ((bcomp b False 3) ++ (ccomp c)) + 1))]
