module Main where

import System.Environment
import Compiler
import Interpreter
import Machine

--TODO Task 3.4
main :: IO ()
main = do
   args <- getArgs
   putStrLn (show ((ccomp (read (head args)::Com))))
