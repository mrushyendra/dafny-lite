module GuardedCommands where

import Language

-- Intermediate program representation without any loops
data GC = Assume Assertion
        | Assert Assertion
        | Havoc Name
        | Cons GC GC
        | NonDet GC GC
        | Skip

class GCTranslator a where
    toGC :: a -> GC

instance GCTranslator Program where
    toGC prog = progToGC prog

-- | Convert a Program to Guarded Commands
progToGC :: Program -> GC
progToGC (Program _ pre post cmds) =
    let postGC = postToGC post
        cmdsGC = cmdsToGC postGC cmds
        progGC = preToGC cmdsGC pre
    in progGC

-- | Convert Post Conditions to Guarded Commands
postToGC :: Post -> GC
postToGC p = Assert (conjoinAssns p)

-- Takes in a Post Condition `postGC` in GC form and a Block `cmds`. Converts `cmds` to Guarded Commands, combining it with `postGC`
cmdsToGC :: GC -> Block -> GC
cmdsToGC postGC cmds = Cons (toGC cmds) postGC

-- Takes in an existing GC `cmdsGC` and a Pre Condition `p`. Converts `p` to Guarded Commands and combines it with `cmdsGC`
preToGC :: GC -> Pre -> GC
preToGC cmdsGC p = Cons (Assume (conjoinAssns p)) cmdsGC

instance GCTranslator [a] where
    toGC ls = Skip

-- instance GCTranslator Statement where
