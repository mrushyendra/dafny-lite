module Main where

import Language
import Parser.Lexer
import Parser.Parser

import System.Environment

-- Outputs AST of program written in IMP language
main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as)
    let parsedProg = parseProg prog
    print "Parsed Prog: "
    print $ parsedProg
