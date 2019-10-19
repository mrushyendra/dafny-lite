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
               | ArrEq Name Name ArithExp ArithExp -- equality between Array, and an Array that has element written to
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

data Statement = Assign Name ArithExp
               | ParAssign Name Name ArithExp ArithExp
               | Write Name ArithExp ArithExp
               | If BoolExp Block Block
               | While BoolExp Inv Block
               deriving (Show)

type Block = [Statement]

data Program = Program Name Pre Post Block deriving (Show)

conjoinAssns :: [Assertion] -> Assertion
conjoinAssns [a] = a
conjoinAssns (a:as) = AConj a (conjoinAssns as)
conjoinAssns _ = ATrue

-- | Replaces all occurrence of Name `old` with `new` in the provided ArithExp
subName :: ArithExp -> Name -> Name -> ArithExp
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

-- Given a Boolexp, returns an equivalent expression in the Assertion Language
boolExpToAssn :: BoolExp -> Assertion
boolExpToAssn (BCmp comp) = AComp comp
boolExpToAssn (BNot bexp) = ANot (boolExpToAssn bexp)
boolExpToAssn (BDisj bexp1 bexp2) = ADisj (boolExpToAssn bexp1) (boolExpToAssn bexp2)
boolExpToAssn (BConj bexp1 bexp2) = AConj (boolExpToAssn bexp1) (boolExpToAssn bexp2)
boolExpToAssn (BParens bexp) = AParens (boolExpToAssn bexp)

-- Returns Set of modified var names
class ModifiedVarNames a where
    modifiedVarNames :: a -> S.Set Name

instance ModifiedVarNames Statement where
    modifiedVarNames (Assign n _) = S.singleton n
    modifiedVarNames (ParAssign n1 n2 _ _) = S.fromList [n1, n2]
    modifiedVarNames (Write n _ _) = S.singleton n
    modifiedVarNames (If _ blk1 blk2) = S.union (modifiedVarNames blk1) (modifiedVarNames blk2)
    modifiedVarNames (While _ _ blk) = modifiedVarNames blk

instance ModifiedVarNames a => ModifiedVarNames [a] where
    modifiedVarNames [] = S.empty
    modifiedVarNames [x] = modifiedVarNames x
    modifiedVarNames (x:xs) = S.union (modifiedVarNames x) (modifiedVarNames xs)

