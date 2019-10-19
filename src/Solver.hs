module Solver where

import Language
import NameGen
import qualified Data.List.NonEmpty as NL
import qualified Data.Set as S

-- | The following types correspond to a minimal subset of the Z3 syntax
newtype Script = Script [Commands] deriving (Show)

data Commands = DeclareConst Symbol Sort
              | Assert Term
              | CheckSat
              deriving (Show)

data Term = Forall (NL.NonEmpty SortedVar) Term
          | Exists (NL.NonEmpty SortedVar) Term
          | App PredefinedFn [Term]
          | TParens Term
          | Numeral Int
          | Const Symbol
          | STrue
          | SFalse
          deriving (Show)

data PredefinedFn = FAdd | FSub | FMul | FDiv | FMod | FEq | FNeq | FLe | FGe | FLt | FGt | FAnd | FOr | FNot | FImplies 
                  | FSelect | FStore deriving (Show)

type Symbol = String

-- Analogous to Types
data Sort = SInt 
          | SBool
          | SArray Sort Sort
          deriving (Show)

data SortedVar = SortedVar Symbol Sort deriving (Show)

-- | Convert Assertion to a Script that can be sent to Z3
assnToScript :: Assertion -> NameGen -> Script
assnToScript assn _ = Script $ (declConsts assn) ++ [Assert $ assnToTerm assn] ++ [CheckSat]

-- | Declare names of all variables in Assertion
declConsts :: Assertion -> [Commands]
declConsts assn =
    let ns = S.toList $ names assn
    in map (\(n, typ) -> DeclareConst n (typToSort typ)) ns

-- | Convert Assertion to a Z3 term
assnToTerm :: Assertion -> Term
assnToTerm (AComp cmp) = compToTerm cmp
assnToTerm (ANot assn) = App FNot [assnToTerm assn]
assnToTerm (ArrEq n1 n2 aexp1 aexp2) = App FEq [nameToTerm n1, (App FStore [(nameToTerm n2), (aexpToTerm aexp1), (aexpToTerm aexp2)])]
assnToTerm (ADisj assn1 assn2) = App FOr [(assnToTerm assn1), (assnToTerm assn2)]
assnToTerm (AConj assn1 assn2) = App FAnd [(assnToTerm assn1), (assnToTerm assn2)]
assnToTerm (AImplies assn1 assn2) = App FImplies [(assnToTerm assn1), (assnToTerm assn2)]
assnToTerm (AForall ns assn) = Forall (namesToSortedVars ns) (assnToTerm assn)
assnToTerm (AExists ns assn) = Exists (namesToSortedVars ns) (assnToTerm assn)
assnToTerm (AParens assn) = TParens (assnToTerm assn)
assnToTerm ATrue = STrue
assnToTerm AFalse = SFalse

-- | Convert Name to a Z3 term
nameToTerm :: Name -> Term
nameToTerm n = Const n

-- | Convert Name to a SortedVar (typed Var)
namesToSortedVars :: [Name] -> NL.NonEmpty SortedVar
namesToSortedVars [n] = SortedVar n SInt NL.:| []
namesToSortedVars (n:ns) = (SortedVar n SInt) NL.:| (namesToSortedVars' ns)
namesToSortedVars [] = error "Forall or Exists cannot be empty"

namesToSortedVars' :: [Name] -> [SortedVar]
namesToSortedVars' [] = []
namesToSortedVars' [n] = [SortedVar n SInt]
namesToSortedVars' (n:ns) = (SortedVar n SInt) : (namesToSortedVars' ns)

-- | Convert Comparison to a Z3 term
compToTerm :: Comparison -> Term
compToTerm (Eq aexp1 aexp2) = App FEq [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Neq aexp1 aexp2) = App FNeq [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Le aexp1 aexp2) = App FLe [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Ge aexp1 aexp2) = App FGe [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Lt aexp1 aexp2) = App FLt [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Gt aexp1 aexp2) = App FGt [(aexpToTerm aexp1), (aexpToTerm aexp2)]

-- | Convert ArithExp to a Z3 Term
aexpToTerm :: ArithExp -> Term
aexpToTerm (Num i) = Numeral i
aexpToTerm (Var n) = Const n
aexpToTerm (Read n aexp) = App FSelect [(nameToTerm n), (aexpToTerm aexp)]
aexpToTerm (Add aexp1 aexp2) = App FAdd [(aexpToTerm aexp1), (aexpToTerm aexp2)]
aexpToTerm (Sub aexp1 aexp2) = App FSub [(aexpToTerm aexp1), (aexpToTerm aexp2)]
aexpToTerm (Mul aexp1 aexp2) = App FMul [(aexpToTerm aexp1), (aexpToTerm aexp2)]
aexpToTerm (Div aexp1 aexp2) = App FDiv [(aexpToTerm aexp1), (aexpToTerm aexp2)]
aexpToTerm (Mod aexp1 aexp2) = App FMod [(aexpToTerm aexp1), (aexpToTerm aexp2)]
aexpToTerm (Parens aexp) = TParens (aexpToTerm aexp)

-- | Correspondence between Imp Language Types and Z3 Sort
typToSort :: Type -> Sort
typToSort GCInt = SInt
typToSort GCArr = SArray SInt SInt -- only 1 type of array in the IMP Language
