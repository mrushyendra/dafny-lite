module NameGen where

import Language
import qualified Data.Map as M

data NameGen = NameGen { maxForName :: M.Map Name Integer }

initNameGen :: NameGen
initNameGen = NameGen { maxForName = M.empty }

-- Generates a new unique Name from seed `n` by appending a unique integer to it
freshSeededName :: Name -> NameGen -> (Name, NameGen)
freshSeededName n ng@(NameGen {maxForName = mn}) =
    case (M.lookup n mn) of
        (Just maxNum) ->
            let maxNum' = maxNum + 1
            in (n ++ (show maxNum'), ng {maxForName = M.insert n maxNum' mn} )
        Nothing -> (n ++ (show 0), ng { maxForName = M.insert n 0 mn} )

-- Generates a new unique Name from seed `n` by appending "a" and a unique integer to it
havocName :: Name -> NameGen -> (Name, NameGen)
havocName n ng@(NameGen {maxForName = mn}) =
    case (M.lookup n mn) of
        (Just maxNum) ->
            let maxNum' = maxNum + 1
            in (n ++ "a" ++ (show maxNum'), ng { maxForName = M.insert n maxNum' mn} )
        Nothing -> (n ++ "a" ++ (show 0), ng { maxForName = M.insert n 0 mn} )
