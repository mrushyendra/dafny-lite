module Language where

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
             | ADisj Assertion Assertion
             | AConj Assertion Assertion
             | AImplies Assertion Assertion
             | AForall [Name] Assertion
             | AExists [Name] Assertion
             | AParens Assertion
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
conjoinAssns _ = error "Empty Assertions"

-- | Replaces all occurrence of Name `oldName` with `newName` in the provided ArithExp
subName :: ArithExp -> Name -> Name -> ArithExp
subName r@(Num _) _ _ =  r
subName (Var n) oldName newName = if (n == oldName) then (Var newName) else (Var n)
subName (Read n aexp) oldName newName = if (n == oldName)
    then Read newName (subName aexp oldName newName)
    else Read n (subName aexp oldName newName)
subName (Add aexp1 aexp2) oldName newName = Add (subName aexp1 oldName newName) (subName aexp2 oldName newName)
subName (Sub aexp1 aexp2) oldName newName = Sub (subName aexp1 oldName newName) (subName aexp2 oldName newName)
subName (Mul aexp1 aexp2) oldName newName = Mul (subName aexp1 oldName newName) (subName aexp2 oldName newName)
subName (Div aexp1 aexp2) oldName newName = Div (subName aexp1 oldName newName) (subName aexp2 oldName newName)
subName (Mod aexp1 aexp2) oldName newName = Mod (subName aexp1 oldName newName) (subName aexp2 oldName newName)
subName (Parens aexp) oldName newName = Parens (subName aexp oldName newName)

