{-# LANGUAGE FlexibleInstances #-}

module Language where

import qualified Data.Set as S

type Name = String

-- | Arithmetic expressions
data ArithExp = Num Int
              | Var Name
              | Read Name ArithExp
              | Add ArithExp ArithExp
              | Sub ArithExp ArithExp
              | Mul ArithExp ArithExp
              | Div ArithExp ArithExp
              | Mod ArithExp ArithExp
              | Parens ArithExp
              deriving (Show)

-- | Comparisons of arithmetic expressions
data Comparison = Eq ArithExp ArithExp
                | Neq ArithExp ArithExp
                | Le ArithExp ArithExp
                | Ge ArithExp ArithExp
                | Lt ArithExp ArithExp
                | Gt ArithExp ArithExp
                deriving (Show)

-- | Boolean expressions 
data BoolExp = BCmp Comparison
             | BNot BoolExp
             | BDisj BoolExp BoolExp
             | BConj BoolExp BoolExp
             | BParens BoolExp
             deriving (Show)

-- | Assertion Language
data Assertion = AComp Comparison
               | ANot Assertion
               | ArrNMEq Name Name
               | ArrEq Name Name ArithExp ArithExp -- analagous to a = store (tmp, idx, val)
               | ADisj Assertion Assertion
               | AConj Assertion Assertion
               | AImplies Assertion Assertion
               | AForall [Name] Assertion
               | AExists [Name] Assertion
               | AParens Assertion
               | ATrue
               | AFalse
               deriving (Show)

-- | Preconditions, Postconditions and Invariants
type Pre = [Assertion]
type Post = [Assertion]
type Inv = [Assertion]

-- | Program Commands
data Statement = Assign Name ArithExp
               | ParAssign Name Name ArithExp ArithExp
               | Write Name ArithExp ArithExp
               | If BoolExp Block Block
               | While BoolExp Inv Block
               deriving (Show)

type Block = [Statement]

data Program = Program Name Pre Post Block deriving (Show)

-- | Types are inferred from the context in which they appear
data Type = GCInt | GCArr deriving (Eq, Ord, Show)

conjoinAssns :: [Assertion] -> Assertion
conjoinAssns [a] = a
conjoinAssns (a:as) = AConj a (conjoinAssns as)
conjoinAssns _ = ATrue

-- Given a Boolexp, returns an equivalent expression in the Assertion Language
boolExpToAssn :: BoolExp -> Assertion
boolExpToAssn (BCmp comp) = AComp comp
boolExpToAssn (BNot bexp) = ANot (boolExpToAssn bexp)
boolExpToAssn (BDisj bexp1 bexp2) = ADisj (boolExpToAssn bexp1) (boolExpToAssn bexp2)
boolExpToAssn (BConj bexp1 bexp2) = AConj (boolExpToAssn bexp1) (boolExpToAssn bexp2)
boolExpToAssn (BParens bexp) = AParens (boolExpToAssn bexp)

class Names a where
    subName :: a -> Name -> Name -> a -- Replaces all occurrences of old Name with new
    names :: a -> S.Set (Name, Type) -- Returns Set of all Names, along with their Type

-- | Replaces all occurrence of Name `old` with `new` in the provided Assertion
instance Names Assertion where
    subName (AComp comp) old new = AComp $ subName comp old new
    subName (ANot assn) old new = ANot $ subName assn old new
    subName (ArrNMEq n1 n2) old new = ArrNMEq (subName n1 old new) (subName n2 old new)
    subName (ArrEq n1 n2 aexp1 aexp2) old new =
        let n1' = if (n1 == old) then new else n1
            n2' = if (n2 == old) then new else n2
        in (ArrEq n1' n2' (subName aexp1 old new) (subName aexp2 old new))
    subName (ADisj assn1 assn2) old new = ADisj (subName assn1 old new) (subName assn2 old new)
    subName (AConj assn1 assn2) old new = AConj (subName assn1 old new) (subName assn2 old new)
    subName (AImplies assn1 assn2) old new = AImplies (subName assn1 old new) (subName assn2 old new)
    subName (AForall ns assn) old new = AForall (subNames ns old new) (subName assn old new)
    subName (AExists ns assn) old new = AExists (subNames ns old new) (subName assn old new)
    subName (AParens assn) old new = AParens (subName assn old new)
    subName x _ _ = x

    names (AComp comp) = names comp
    names (ANot assn) = names assn
    names (ArrNMEq n1 n2) = S.fromList [(n1, GCArr), (n2, GCArr)]
    names (ArrEq n1 n2 aexp1 aexp2) = S.union (S.fromList [(n1, GCArr), (n2, GCArr)]) $ S.union (names aexp1) (names aexp2)
    names (ADisj assn1 assn2) =  S.union (names assn1) (names assn2)
    names (AConj assn1 assn2) =  S.union (names assn1) (names assn2)
    names (AImplies assn1 assn2) = S.union (names assn1) (names assn2)
    names (AForall ns assn) = S.union (S.fromList (map (\n -> (n, GCInt)) ns)) (names assn) -- assume all names in `ns` are Ints
    names (AExists ns assn) = S.union (S.fromList (map (\n -> (n, GCInt)) ns)) (names assn)
    names (AParens assn) = names assn
    names _ = S.empty

-- | Replaces all occurrence of Name `old` with `new` in the provided ArithExp
instance Names ArithExp where
    subName r@(Num _) _ _ =  r
    subName (Var n) old new = if (n == old) then (Var new) else (Var n)
    subName (Read n aexp) old new = if (n == old)
        then Read new (subName aexp old new)
        else Read n (subName aexp old new)
    subName (Add aexp1 aexp2) old new = Add (subName aexp1 old new) (subName aexp2 old new)
    subName (Sub aexp1 aexp2) old new = Sub (subName aexp1 old new) (subName aexp2 old new)
    subName (Mul aexp1 aexp2) old new = Mul (subName aexp1 old new) (subName aexp2 old new)
    subName (Div aexp1 aexp2) old new = Div (subName aexp1 old new) (subName aexp2 old new)
    subName (Mod aexp1 aexp2) old new = Mod (subName aexp1 old new) (subName aexp2 old new)
    subName (Parens aexp) old new = Parens (subName aexp old new)

    names (Num _) = S.empty
    names (Var n) = S.singleton (n, GCInt)
    names (Read n aexp) = S.union (S.singleton (n, GCArr)) (names aexp)
    names (Add aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Sub aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Mul aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Div aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Mod aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Parens aexp) = names aexp

instance Names Comparison where
    subName (Eq aexp1 aexp2) old new = Eq (subName aexp1 old new) (subName aexp2 old new)
    subName (Neq aexp1 aexp2) old new = Neq (subName aexp1 old new) (subName aexp2 old new)
    subName (Le aexp1 aexp2) old new = Le (subName aexp1 old new) (subName aexp2 old new)
    subName (Ge aexp1 aexp2) old new = Ge (subName aexp1 old new) (subName aexp2 old new)
    subName (Lt aexp1 aexp2) old new = Lt (subName aexp1 old new) (subName aexp2 old new)
    subName (Gt aexp1 aexp2) old new = Gt (subName aexp1 old new) (subName aexp2 old new)

    names (Eq aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Neq aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Le aexp1 aexp2) =  S.union (names aexp1) (names aexp2)
    names (Ge aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Lt aexp1 aexp2) = S.union (names aexp1) (names aexp2)
    names (Gt aexp1 aexp2) = S.union (names aexp1) (names aexp2)

subNames :: [Name] -> Name -> Name -> [Name]
subNames (x:xs) old new =
    let x' = subName x old new
    in x':(subNames xs old new)
subNames [] _ _ = []

instance Names Name where
    subName curr old new = if (curr == old) then new else curr
    names _ = S.empty -- not used

-- Returns Set of modified var names
class ModifiedVarNames a where
    modifiedVarNames :: a -> S.Set (Name, Type)

instance ModifiedVarNames Statement where
    modifiedVarNames (Assign n _) = S.singleton (n, GCInt)
    modifiedVarNames (ParAssign n1 n2 _ _) = S.fromList [(n1, GCInt), (n2, GCInt)]
    modifiedVarNames (Write n _ _) = S.singleton (n, GCArr)
    modifiedVarNames (If _ blk1 blk2) = S.union (modifiedVarNames blk1) (modifiedVarNames blk2)
    modifiedVarNames (While _ _ blk) = modifiedVarNames blk

instance ModifiedVarNames a => ModifiedVarNames [a] where
    modifiedVarNames [] = S.empty
    modifiedVarNames [x] = modifiedVarNames x
    modifiedVarNames (x:xs) = S.union (modifiedVarNames x) (modifiedVarNames xs)
