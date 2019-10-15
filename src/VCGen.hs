module Main where

import Language
import Parser.Lexer
import Parser.Parser

import System.Environment

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as)
    let parsedProg = parseProg prog

    -- change to guarded commands
    -- do wp
    -- send to smt solver

    print $ parsedProg
