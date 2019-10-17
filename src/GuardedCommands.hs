module GuardedCommands where

import Language
import NameGen

-- Intermediate program representation without any loops
data GC = Assume Assertion
        | Assert Assertion
        | Havoc Name
        | Cons GC GC
        | NonDet GC GC
        | Skip
        deriving (Show)

class GCTranslator a where
    toGC :: a -> NameGen -> (GC, NameGen)

instance GCTranslator Program where
    toGC prog ng = progToGC prog ng

-- | Convert a Program to Guarded Commands
progToGC :: Program -> NameGen -> (GC, NameGen)
progToGC (Program _ pre post cmds) ng =
    let postGC = postToGC post
        (cmdsGC, ng') = cmdsToGC postGC cmds ng
        progGC = preToGC cmdsGC pre
    in (progGC, ng')

-- | Convert Post Conditions to Guarded Commands
postToGC :: Post -> GC
postToGC p = Assert (conjoinAssns p)

-- Takes in a Post Condition `postGC` in GC form and a Block `cmds`. Converts `cmds` to Guarded Commands, combining it with `postGC`
cmdsToGC :: GC -> Block -> NameGen -> (GC, NameGen)
cmdsToGC postGC cmds ng =
    let (cmdsGC, ng') = toGC cmds ng
    in (Cons cmdsGC postGC, ng')

-- Takes in an existing GC `cmdsGC` and a Pre Condition `p`. Converts `p` to Guarded Commands and combines it with `cmdsGC`
preToGC :: GC -> Pre -> GC
preToGC cmdsGC p = Cons (Assume (conjoinAssns p)) cmdsGC

instance GCTranslator a => GCTranslator [a] where
    toGC [] ng = (Skip, ng)
    toGC [x] ng = toGC x ng
    toGC (x:xs) ng =
        let (xGC, ng') = toGC x ng
            (xsGC, ng'') = toGC x ng'
        in (Cons xGC xsGC, ng'')

instance GCTranslator Statement where
    toGC = statementToGC

statementToGC :: Statement -> NameGen -> (GC, NameGen)
statementToGC (Assign n aexp) ng =
    let (tmp, ng') = freshSeededName n ng
    in (Cons (Assume (AComp (Eq (Var tmp) (Var n))))
            (Cons (Havoc n)
                (Assume (AComp (Eq (Var n) (subName aexp n tmp))))), ng')
