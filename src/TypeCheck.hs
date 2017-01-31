{-# LANGUAGE ScopedTypeVariables #-}    -- Allows type annotations on variables
                                        -- in do-notation
module TypeCheck where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (foldM)

import Util
import Syntax

-----------------------------------
-- Machinery for name generation --
-----------------------------------

type NameGen = Int

genNNames :: Int -> NameGen -> ([String], NameGen)
genNNames 0 gen = ([], gen)
genNNames n gen = let
    (rest   , genAfterRest) = genNNames (n - 1) gen
    (newName, genLast     ) = genName genAfterRest
    in
    (newName : rest, genLast)

genName :: NameGen -> (String, NameGen)
genName g = ("__" ++ alphaFromInteger g ++ "__", g + 1)
    where

    -- Generate a string of lowercase alphabetic characters from an integer by
    -- interpreting the integer in base 26 such that a 0 is a, 1 is b, etc.
    -- Generation is in reverse order so that the leading digit changes with
    -- addition of one, helping to distinguish variable names.
    alphaFromInteger :: Int -> String
    alphaFromInteger = map (\digit -> ['a'..'z'] !! digit) . digits 26

    -- Get the digits of a number in a given base, in reverse order
    digits :: Int -> Int -> [Int]
    digits base x | x == 0 = [0]
    digits base x | True   = ds base x where
        ds base x | x == 0 = []
                  | True   = (x `mod` base) : ds base (x `div` base)

-----------------------
-- Environment types --
-----------------------

type Env = M.Map Name Type

-- Query the type of a specific variable in an environment. Fails if the
-- variable is not found.
(?) :: Env -> Name -> Result Type
env ? n = case M.lookup n env of
    Nothing -> Error $ "Env lookup failed for variable '" ++ n ++ "'"
    Just t  -> return t

-- Update the binding for a (Name, Type) binding in an environment
(+=) :: Env -> (Name, Type) -> Env
env += (n, t) = M.insert n t env

-- Convert a TCDec into an Env containing type mappings for the functions
-- in the typeclass declaration
envFromTCDec :: TCDec -> Env
envFromTCDec (TCDec classN memberN funcs) =
    M.map (TQuant memberN (S.singleton classN)) funcs

----------------------
-- Constraint types --
----------------------

type Constraint = (Type, Type)

----------------------------------
-- Type checking implementation --
----------------------------------

unify :: S.Set Constraint -> Result (Type -> Type)
unify = undefined

constraintsProg :: NameGen -> Prog -> Result (Type, S.Set Constraint, NameGen)
constraintsProg gen prog = let

    allInClassFuncNames :: [Name]
    allInClassFuncNames = concatMap (M.keys . functions) $ getTCDecs prog

    hasDuplicateInClassFuncNames :: Bool
    hasDuplicateInClassFuncNames =
        S.size (S.fromList allInClassFuncNames) < length allInClassFuncNames

    in if hasDuplicateInClassFuncNames then
        Error "Conflicting function definitions in typeclass declarations"
    else let

    envBindingsFromTCDecs :: Env
    envBindingsFromTCDecs = M.unions $ map envFromTCDec $ getTCDecs prog

    fnDecs :: [FNDec]
    fnDecs = getFNDecs prog

    fnNames :: [Name]
    fnNames = map fnName fnDecs

    hasDuplicateFuncNames :: Bool
    hasDuplicateFuncNames = S.size (S.fromList fnNames) < length fnNames

    in if hasDuplicateFuncNames then
        Error "Conflicting function definitions at top level"
    else let

    tVarNamesForFuncs :: [Name]
    newGen :: NameGen
    (tVarNamesForFuncs, newGen) = genNNames (length fnDecs) gen

    envBindingsFromFNDecs :: Env
    envBindingsFromFNDecs =
        M.fromList $ zip fnNames $ map TVar tVarNamesForFuncs

    completeEnv :: Env
    completeEnv = M.union envBindingsFromFNDecs envBindingsFromTCDecs

    --tiFuncs :: [FNDec] ->

    in do

    constraintGenResultsForFunctions :: (S.Set Constraint, NameGen) <-
        foldM ( \(constraintsAll, gen) fnDec -> do
            (constraintsFn, genAfter) <- constraintsFNDec completeEnv gen fnDec
            return (S.union constraintsAll constraintsFn, genAfter)
        ) (S.empty, newGen) fnDecs

    {-constraintGenResultsForClassWitnesses :: (S.Set Constraint, NameGen) <-
        foldM ( \(constraintsAll, gen) fnDec -> do
            (constraintsFn, genAfter) <- constraintsFNDec completeEnv gen fnDec
            return (S.union constraintsAll constraintsFn, genAfter)
        ) (S.empty, newGen) undefined
-}
    return (undefined, undefined, undefined)

-- The given function expects a binding for its own name already in the Env
-- via a fresh type variable. constraintsProg adds this.
constraintsFNDec :: Env -> NameGen -> FNDec ->
    Result (S.Set Constraint, NameGen)
constraintsFNDec env gen (FNDec n ty args body) = let
    (argNames, newGen) = genNNames (length args) gen
    envWithAll = M.unionWith (\_ x -> x) env $
        M.fromList $ zip args $ map TVar argNames 
    in do
    tyThis <- env ? n
    (tyBody, conBody, newerGen) <- constraintsExp envWithAll newGen body
    return (S.insert (ty, tyThis) conBody, newerGen)

constraintsExp :: Env -> NameGen -> Exp ->
    Result (Type, S.Set Constraint, NameGen)
constraintsExp env gen exp = case exp of
    App f a -> do
        let (newName, newGen) = genName gen
        let tyR = TVar newName
        (tyF, conF, genAfterF) <- constraintsExp env newGen f
        (tyA, conA, genAfterA) <- constraintsExp env genAfterF a
        let conAll = (tyF, TFunc tyA tyR) `S.insert` conF `S.union` conA
        return (tyR, conAll, newGen)
    Let n x y -> do
        (tyX, conX, genAfterX) <- constraintsExp env gen x
        (tyY, conY, genAfterY) <- constraintsExp (env += (n, tyX)) genAfterX y
        return (tyY, conX `S.union` conY, genAfterY)
    Lam n x -> do
        let (newName, newGen) = genName gen
        (tyX, conX, genAfterX) <-
            constraintsExp (env += (n, TVar newName)) newGen x
        return (TFunc (TVar newName) tyX, conX, genAfterX)
    Var x -> do
        tyX <- env ? x
        return (tyX, S.empty, gen)
    Const _ -> return (TInt, S.empty, gen)
    Prod e f -> do
        (tyE, conE, genAfterE) <- constraintsExp env gen e
        (tyF, conF, genAfterF) <- constraintsExp env genAfterE f
        return (TProd tyE tyF, conE `S.union` conF, genAfterF)
    Fst -> return (tForAll ["a", "b"] $
        TFunc (TProd (TVar "a") (TVar "b")) $ TVar "a", S.empty, gen)
    Snd -> return (tForAll ["a", "b"] $
        TFunc (TProd (TVar "a") (TVar "b")) $ TVar "b", S.empty, gen)
    Add -> return (TFunc TInt $ TFunc TInt TInt, S.empty, gen)
    Sub -> return (TFunc TInt $ TFunc TInt TInt, S.empty, gen)
