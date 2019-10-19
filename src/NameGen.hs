module NameGen where

import Language
import qualified Data.Map as M

-- | For each Type (Int or Arr), maps a seed Name (e.g. 'a') to the largest integer appended to any 
-- name generated from the integer so far (e.g. 'a7')
data NameGen = NameGen { intNameMax :: M.Map Name Integer, arrNameMax :: M.Map Name Integer }

initNameGen :: NameGen
initNameGen = NameGen { intNameMax = M.empty, arrNameMax = M.empty }

-- | Generates a new unique Name corresponding to an Int, from seed `n` by appending a unique integer to it
freshSeededIntName :: Name -> NameGen -> (Name, NameGen)
freshSeededIntName n ng@(NameGen {intNameMax = mn}) =
    let (new, mn') = freshSeededName n mn ""
    in (new, ng { intNameMax = mn' })

-- | Generates a new unique Name corresponding to an Array, from seed `n` by appending a unique integer to it
freshSeededArrName :: Name -> NameGen -> (Name, NameGen)
freshSeededArrName n ng@(NameGen {arrNameMax = mn}) =
    let (new, mn') = freshSeededName n mn "arr"
    in (new, ng { arrNameMax = mn' })

freshSeededName :: Name -> M.Map Name Integer -> String -> (Name, M.Map Name Integer)
freshSeededName n mn txt =
    case (M.lookup n mn) of
        (Just maxNum) ->
            let maxNum' = maxNum + 1
            in (txt ++ n ++ (show maxNum'), M.insert n maxNum' mn)
        Nothing -> (txt ++ n ++ (show (0::Integer)), M.insert n 0 mn)

-- | Returns list of all Names corresponding to Ints that have been generated so far
intNames :: NameGen -> [Name]
intNames (NameGen {intNameMax = mn})= M.foldrWithKey' (\nm maxVal ns -> (generateNames nm 0 maxVal) ++ ns) [] mn

-- | Returns list of all Names corresponding to Ints that have been generated so far
arrNames :: NameGen -> [Name]
arrNames (NameGen {arrNameMax = mn})= M.foldrWithKey' (\nm maxVal ns -> (generateNames nm 0 maxVal) ++ ns) [] mn

-- | Given a Seed Name `nm` and a range [min, max], returns a list of all Names formed by appending each number i in the range to the Seed
generateNames :: Name -> Integer -> Integer -> [Name]
generateNames nm lo hi =
    if (lo > hi)
        then []
        else
            let newName = nm ++ (show lo)
                lo' = lo + 1
            in (newName : (generateNames nm lo' hi))
