module Main where

import Language
import NameGen
import GuardedCommands
import Parser.Lexer
import Parser.Parser

import System.Environment

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as)
    let parsedProg = parseProg prog
        ng = initNameGen
        (guardedCmds, _) = toGC parsedProg ng
    -- calculate wp
    -- send to smt solver

    print parsedProg
    print "Guarded Commands: "
    print $ guardedCmds
