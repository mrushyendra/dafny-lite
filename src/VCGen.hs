module Main where

import Language
import GuardedCommands
import Parser.Lexer
import Parser.Parser

import System.Environment

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as)
    let parsedProg = parseProg prog
        guardedCmds = toGC parsedProg
    -- calculate wp
    -- send to smt solver

    print $ parsedProg
