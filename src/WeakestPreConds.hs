module WeakestPreConds where

import Language
import GuardedCommands
import NameGen

-- | Given a Guarded Command representing a Program, and a final assertion, computes the Weakest Precondition for the Program
computeWeakestPre :: GC -> Assertion -> NameGen -> (Assertion, NameGen)
computeWeakestPre (Assume b) assn ng = (AImplies b assn, ng)
computeWeakestPre (Assert b) assn ng = (AConj b assn, ng)
computeWeakestPre (Havoc x t) assn ng =
    let (fresh, ng') = case t of
            GCInt -> freshSeededIntName x ng
            GCArr -> freshSeededArrName x ng
    in (subName assn x fresh, ng')
computeWeakestPre (Cons gc1 gc2) assn ng = 
    let (wp1, ng') = computeWeakestPre gc2 assn ng
    in (computeWeakestPre gc1 wp1 ng')
computeWeakestPre (NonDet gc1 gc2) assn ng =
    let (wp1, ng') = computeWeakestPre gc1 assn ng
        (wp2, ng'') = computeWeakestPre gc2 assn ng'
    in (AConj wp1 wp2, ng'')
computeWeakestPre Skip assn ng = (assn, ng)

