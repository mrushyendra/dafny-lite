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
        -- convert program to AST
        parsedProg = parseProg prog
        (guardedCmds, ng') = toGC parsedProg ng
        -- compute logical formula representing weakest preconditions in order for postconditions to hold, and negate it
        -- if negated formula is unsatisfiable, then formula is Valid
        (wp, ng'') = computeWeakestPre guardedCmds ng'
        -- convert to Z3 formula representation
        wpScript = assnToScript wp ng''
        wpZ3 = show wpScript

    (stdin_hdl, stdout_hdl, _) <- createZ3Process
    res <- callZ3 wpZ3 stdin_hdl stdout_hdl

    {- print "ParsedProg: "
    print parsedProg
    print "Guarded Commands: "
    print guardedCmds
    print "Weakest Precondition: "
    print wp
    print "Input to Z3: "
    print wpScript
    print "Res: "-}
    print res
