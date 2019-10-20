module Main where

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
    let ng = initNameGen
        parsedProg = parseProg prog
        (guardedCmds, ng') = toGC parsedProg ng
        (wp, ng'') = computeWeakestPre guardedCmds ng'
        wpScript = assnToScript wp ng''
        wpZ3 = show wpScript

    (stdin_hdl, stdout_hdl, _) <- createZ3Process
    res <- callZ3 wpZ3 stdin_hdl stdout_hdl

    print parsedProg
    print "Guarded Commands: "
    print guardedCmds
    print "Weakest Precondition: "
    print wp
    print "Input to Z3: "
    print wpScript
    print "Res: "
    print res
