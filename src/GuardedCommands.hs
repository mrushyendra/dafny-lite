module GuardedCommands where

import Language
import NameGen
import qualified Data.Set as S

-- Intermediate program representation without any loops
data GC = Assume Assertion
        | Assert Assertion
        | Havoc Name Type
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
postToGC [] = Assert (ATrue)
postToGC p = Assert (conjoinAssns p)

-- Takes in a Post Condition `postGC` in GC form and a Block `cmds`. Converts `cmds` to Guarded Commands, combining it with `postGC`
cmdsToGC :: GC -> Block -> NameGen -> (GC, NameGen)
cmdsToGC postGC cmds ng =
    let (cmdsGC, ng') = toGC cmds ng
    in (Cons cmdsGC postGC, ng')

-- Takes in an existing GC `cmdsGC` and a Pre Condition `p`. Converts `p` to Guarded Commands and combines it with `cmdsGC`
preToGC :: GC -> Pre -> GC
preToGC cmdsGC [] = Cons (Assume ATrue) cmdsGC
preToGC cmdsGC p = Cons (Assume (conjoinAssns p)) cmdsGC

instance GCTranslator a => GCTranslator [a] where
    toGC [] ng = (Skip, ng)
    toGC [x] ng = toGC x ng
    toGC (x:xs) ng =
        let (xGC, ng') = toGC x ng
            (xsGC, ng'') = toGC xs ng'
        in (Cons xGC xsGC, ng'')

instance GCTranslator Statement where
    toGC = statementToGC

statementToGC :: Statement -> NameGen -> (GC, NameGen)
statementToGC (Assign n aexp) ng =
    let (tmp, ng') = freshSeededIntName n ng
    in (Cons (Assume (AComp (Eq (Var tmp) (Var n))))
            (Cons (Havoc n GCInt)
                (Assume (AComp (Eq (Var n) (subName aexp n tmp))))), ng')
statementToGC (ParAssign n1 n2 aexp1 aexp2) ng =
    let (gc1, ng') = statementToGC (Assign n1 aexp1) ng
        (gc2, ng'') = statementToGC (Assign n2 aexp2) ng'
    in (Cons gc1 gc2, ng'')
statementToGC (Write n aexp1 aexp2) ng =
    let (tmpArr, ng') = freshSeededArrName n ng
    in (Cons (Assume (ArrNMEq tmpArr n))
            (Cons (Havoc n GCArr)
                (Assume (ArrEq n tmpArr (subName aexp1 n tmpArr) (subName aexp2 n tmpArr)))), ng')
statementToGC (If bexp thenExp elseExp) ng =
    let (thenExpGC, ng') = toGC thenExp ng
        (elseExpGC, ng'') = toGC elseExp ng'
    in ((NonDet (Cons (Assume (boolExpToAssn bexp)) thenExpGC)
            (Cons (Assume (ANot $ boolExpToAssn bexp)) elseExpGC)), ng'')
statementToGC (While bexp inv blk) ng =
    let assn = (Assert $ conjoinAssns inv)
        modified = map (\(n, t) -> Havoc n t) (S.toList $ modifiedVarNames blk)
        assum = (Assume $ conjoinAssns inv)
        (blkGC, ng') = toGC blk ng
        choice = (NonDet (gcListToGC [(Assume (boolExpToAssn bexp)), blkGC, (Assert $ conjoinAssns inv), (Assume AFalse)])
                    (Assume (ANot $ boolExpToAssn bexp)))
        gcList = assn : (modified ++ [assum, choice])
    in (gcListToGC gcList, ng')

-- | Folds list of GCs into a single GC using Cons
gcListToGC :: [GC] -> GC
gcListToGC [x] = x
gcListToGC (x:xs) = Cons x (gcListToGC xs)
gcListToGC _ = error "Cannot map empty list to Guarded Command"

