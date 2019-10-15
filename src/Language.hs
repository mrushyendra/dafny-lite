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

type Program = (Name, Pre, Post, Block)
