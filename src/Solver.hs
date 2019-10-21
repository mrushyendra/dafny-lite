module Solver where

import Language
import NameGen
import System.Process
import System.IO
import qualified Data.List.NonEmpty as NL
import qualified Data.Set as S

-- | The following types correspond to a minimal subset of the Z3 syntax
newtype Script = Script [Command]

data Command = DeclareConst Symbol Sort
              | Assert Term
              | CheckSat

data Term = Forall (NL.NonEmpty SortedVar) Term
          | Exists (NL.NonEmpty SortedVar) Term
          | App PredefinedFn [Term]
          | TParens Term
          | Numeral Int
          | Const Symbol
          | STrue
          | SFalse

data PredefinedFn = FAdd | FSub | FMul | FDiv | FMod | FEq | FLe | FGe | FLt | FGt | FAnd | FOr | FNot | FImplies | FSelect | FStore

type Symbol = String

-- Analogous to Types
data Sort = SInt 
          | SBool
          | SArray Sort Sort

data SortedVar = SortedVar Symbol Sort

-- | Translate Assertion to a Script that can be sent to Z3
assnToScript :: Assertion -> NameGen -> Script
assnToScript assn _ = Script $ (declConsts assn) ++ [Assert $ assnToTerm assn] ++ [CheckSat]

-- | Declare names of all variables in Assertion
declConsts :: Assertion -> [Command]
declConsts assn =
    let ns = S.toList $ names assn
    in map (\(n, typ) -> DeclareConst n (typToSort typ)) ns

-- | Translate Assertion to a Z3 term
assnToTerm :: Assertion -> Term
assnToTerm (AComp cmp) = compToTerm cmp
assnToTerm (ANot assn) = App FNot [assnToTerm assn]
assnToTerm (ArrNMEq n1 n2) = App FEq [nameToTerm n1, nameToTerm n2]
assnToTerm (ArrEq n1 n2 aexp1 aexp2) = App FEq [nameToTerm n1, (App FStore [(nameToTerm n2), (aexpToTerm aexp1), (aexpToTerm aexp2)])]
assnToTerm (ADisj assn1 assn2) = App FOr [(assnToTerm assn1), (assnToTerm assn2)]
assnToTerm (AConj assn1 assn2) = App FAnd [(assnToTerm assn1), (assnToTerm assn2)]
assnToTerm (AImplies assn1 assn2) = App FImplies [(assnToTerm assn1), (assnToTerm assn2)]
assnToTerm (AForall ns assn) = Forall (namesToSortedVars ns) (assnToTerm assn)
assnToTerm (AExists ns assn) = Exists (namesToSortedVars ns) (assnToTerm assn)
assnToTerm (AParens assn) = TParens (assnToTerm assn)
assnToTerm ATrue = STrue
assnToTerm AFalse = SFalse

-- | Translate Name to a Z3 term
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

-- | Translate Comparison to a Z3 term
compToTerm :: Comparison -> Term
compToTerm (Eq aexp1 aexp2) = App FEq [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Neq aexp1 aexp2) = App FNot [App FEq [(aexpToTerm aexp1), (aexpToTerm aexp2)]]
compToTerm (Le aexp1 aexp2) = App FLe [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Ge aexp1 aexp2) = App FGe [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Lt aexp1 aexp2) = App FLt [(aexpToTerm aexp1), (aexpToTerm aexp2)]
compToTerm (Gt aexp1 aexp2) = App FGt [(aexpToTerm aexp1), (aexpToTerm aexp2)]

-- | Translate ArithExp to a Z3 Term
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

-- | Conversion of Z3 syntax to String
instance Show Script where
    show (Script xs) = showElems (map show xs)

instance Show Command where
    show (DeclareConst sym sort) = "(declare-const " ++ sym ++ " " ++ (show sort) ++ ")\n"
    show (Assert t) = "(assert " ++ (show t) ++ ")\n"
    show CheckSat = "(check-sat)\n"

instance Show Term where
    show (Forall sortedVars t) = "(forall (" ++ (showElems $ NL.toList $ NL.map show sortedVars) ++ ") " ++ (show t) ++")"
    show (Exists sortedVars t) = "(exists (" ++ (showElems $ NL.toList $ NL.map show sortedVars) ++ ") " ++ (show t) ++ ")"
    show (App fn ts) = "(" ++ (show fn) ++ " " ++ (showElems $ map show ts) ++ ")"
    show (TParens t) = show t -- No need for Parens anymore
    show (Numeral i) = show i
    show (Const sym) = sym
    show STrue = "true"
    show SFalse = "false"

instance Show PredefinedFn where
    show FAdd = "+"
    show FSub = "-"
    show FMul = "*"
    show FDiv = "div"
    show FMod = "mod"
    show FEq = "="
    show FLe = "<="
    show FGe = ">="
    show FLt = "<"
    show FGt = ">"
    show FAnd = "and"
    show FOr = "or"
    show FNot = "not"
    show FImplies = "=>"
    show FSelect = "select"
    show FStore = "store"

instance Show Sort where
    show SInt = "Int"
    show SBool = "Bool"
    show (SArray s1 s2) = "(Array " ++ (show s1) ++ " " ++ (show s2) ++ ")"

instance Show SortedVar where
    show (SortedVar sym sort) = "(" ++ sym ++ " " ++ (show sort) ++ ")"

showElems :: [String] -> String
showElems [] = ""
showElems [x] = x
showElems (x:xs) = x ++ " " ++ showElems xs

-- | Spawns a Z3 process and creates pipe to it
createZ3Process :: IO (Handle, Handle, ProcessHandle)
createZ3Process = do
    let pr = proc "../z3-4.8.1.016872a5e0f6-x64-ubuntu-16.04/bin/z3" ["-smt2", "-in"]
    (mb_stdin_hdl, mb_stdout_hdl, mb_stderr_hdl, ph) <- createProcess (pr { std_in = CreatePipe, std_out = CreatePipe })

    case mb_stderr_hdl of
        Just stderr_hdl -> hClose stderr_hdl
        Nothing -> return ()

    let stdin_hdl = case mb_stdin_hdl of
            Just x -> x
            Nothing -> error "Unable to open stdin"
        stdout_hdl = case mb_stdout_hdl of
            Just x -> x
            Nothing -> error "Unable to open stdout"

    hSetBuffering stdin_hdl LineBuffering
    return (stdin_hdl, stdout_hdl, ph)

-- | Sends the String representing the computed Weakest Precondition to Z3 and returns "Verified" or "Not Verified" based on Z3's output
callZ3 :: String -> Handle -> Handle -> IO (String)
callZ3 wp stdin_hdl stdout_hdl = do
    hPutStr stdin_hdl wp
    isOutput <- hWaitForInput stdout_hdl (-1)
    res <- case isOutput of
        True -> do
            output <- hGetLine stdout_hdl
            if (output == "sat")
                then return "Not verified"
            else if (output == "unsat")
                then return "Verified"
            else return "UNKNOWN"
        False -> return "UNKNOWN"
    hPutStr stdin_hdl "(exit)"
    return res
