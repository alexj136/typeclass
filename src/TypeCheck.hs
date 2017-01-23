module TypeCheck where

import qualified Data.Set as S
import qualified Data.Map as M

import Util
import Syntax

-----------------------------------
-- Machinery for name generation --
-----------------------------------

type NameGen = Int

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
genConstraints = undefined

constraintsFNDec :: NameGen -> FNDec -> Result (Type, S.Set Constraint, NameGen)
constraintsFNDec = undefined

constraintsExp :: Env -> NameGen -> Exp ->
    Result (Type, S.Set Constraint, NameGen)
constraintsExp env gen exp = case exp of
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
