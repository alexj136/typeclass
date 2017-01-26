module TypeCheck where

import qualified Data.Set as S
import qualified Data.Map as M

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

(?) :: Env -> Name -> Result Type
env ? n = case M.lookup n env of
    Nothing -> Error $ "Env lookup failed for variable '" ++ n ++ "'"
    Just t  -> return t

(+=) :: Env -> (Name, Type) -> Env
env += (n, t) = M.insert n t env

----------------------
-- Constraint types --
----------------------

type Constraint = (Type, Type)

----------------------------------
-- Type checking implementation --
----------------------------------

unify :: S.Set Constraint -> Result (Type -> Type)
unify = undefined

genConstraints :: Prog -> Result (S.Set Constraint)
genConstraints prog = let
    tcDecs = getTCDecs prog
    tiDecs = getTIDecs prog
    fnDecs = getFNDecs prog
    initialEnvWithTCDecFunctions =
        undefined
    in
    undefined

-- Expects itself already in the Env via a fresh type variable
constraintsFNDec :: Env -> NameGen -> FNDec ->
    Result (Type, S.Set Constraint, NameGen)
constraintsFNDec env gen (FNDec n ty args body) = let
    (argNames, newGen) = genNNames (length args) gen
    envWithAll = M.unionWith (\_ x -> x) env $
        M.fromList $ zip args $ map TVar argNames 
    in do
    tyThis <- env ? n
    (tyBody, conBody, newerGen) <- constraintsExp envWithAll newGen body
    return (tyThis, S.insert (ty, tyThis) conBody, newerGen)

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
