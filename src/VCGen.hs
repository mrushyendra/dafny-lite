module Main where

import Language
import NameGen
import GuardedCommands
import WeakestPreConds
import Parser.Lexer
import Parser.Parser

import System.Environment

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as)
    let parsedProg = parseProg prog
        ng = initNameGen
        (guardedCmds, ng') = toGC parsedProg ng
        (wp, ng'') = computeWeakestPre guardedCmds ATrue ng'
    -- send to smt solver

    print parsedProg
    print "Guarded Commands: "
    print guardedCmds
    print "Weakest Precondition: "
    print wp
