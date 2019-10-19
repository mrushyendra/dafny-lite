module Main where

import Language
import NameGen
import Solver
import GuardedCommands
import WeakestPreConds
import Parser.Parser

import System.Environment

main :: IO ()
main = do
    as <- getArgs
    prog <- readFile (head as)
    let parsedProg = parseProg prog
        -- add names to nameGen
        ng = initNameGen
        (guardedCmds, ng') = toGC parsedProg ng
        (wp, ng'') = computeWeakestPre guardedCmds ATrue ng'
        wpZ3 = assnToScript wp ng''
        -- convert to smt-lib (z3) string
        -- send to smt solver

    print parsedProg
    print "Guarded Commands: "
    print guardedCmds
    print "Weakest Precondition: "
    print wp
    print "Z3 Input: "
    print wpZ3
