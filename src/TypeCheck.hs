{-# LANGUAGE ScopedTypeVariables #-}    -- Allows type annotations on variables
                                        -- in do-notation
module TypeCheck where

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (foldM)

import Util
import Syntax

-----------------------
-- Environment types --
-----------------------

type Env = M.Map Name Type

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

type TypeSub = NameGen -> Type -> Result (Type, NameGen)

typeSubIdentity :: TypeSub
typeSubIdentity gen t = return (t, gen)

unify :: NameGen -> S.Set Constraint -> Result (TypeSub, NameGen)
unify gen constrs = if null constrs then return (typeSubIdentity, gen) else
    let (next, rest) = S.deleteFindMin constrs in case next of

        (ty1, ty2) | ty1 == ty2 -> unify gen rest

        (TVar x, ty) | S.notMember x $ frees ty -> handleVar gen x ty rest

        (ty, TVar x) | S.notMember x $ frees ty -> handleVar gen x ty rest

        (TFunc a1 b1, TFunc a2 b2) -> unify gen $
            S.insert (a1, a2) $
            S.insert (b1, b2) $ rest

        (ty1, ty2) -> Error $ "Unsatisfiable type constraint found"

    where

        handleVar :: NameGen -> Name -> Type -> S.Set Constraint ->
            Result (TypeSub, NameGen)
        handleVar gen x ty rest = do
            (subbedRest, gen') <- foldM
                (\(cs, gen) c -> do
                    (c', gen') <- constrSub gen x ty c
                    return (c' : cs, gen'))
                ([], gen)
                (S.toList rest)
            (unifyRest, gen'') <- unify gen' $ S.fromList subbedRest
            return $ (unifyRest >>= (typeSubst x ty), gen'')

        constrSub :: NameGen -> Name -> Type -> Constraint ->
            Result (Constraint, NameGen)
        constrSub gen x ty (t1, t2) = do
            (t1', gen' ) <- typeSubst x ty gen  t1
            (t2', gen'') <- typeSubst x ty gen' t2
            return ((t1', t2'), gen'')

type ConstraintGenResult a = Result (a, S.Set Constraint, NameGen)
type ConstraintGen a b = Env -> NameGen -> a -> ConstraintGenResult b

foldCG :: ConstraintGen a b -> ConstraintGen [a] [b]
foldCG cg env gen = foldM (\(bs, constraints, gen) a -> do
    (b, constraintsA, gen') <- cg env gen a
    return (b : bs, S.union constraints constraintsA, gen')
    ) ([], S.empty, gen)

bindCG :: ConstraintGen a b -> ConstraintGen x y -> ConstraintGen (a, x) (b, y)
bindCG abCGen xyCGen env nameGen (a, x) = do
    (b, constraintsA, nameGen' ) <- abCGen env nameGen  a
    (y, constraintsB, nameGen'') <- xyCGen env nameGen' x
    return ((b, y), S.union constraintsA constraintsB, nameGen'')

constraintsProg :: ConstraintGen Prog Type
constraintsProg env gen prog = let

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
    gen' :: NameGen
    (tVarNamesForFuncs, gen') = genNNames (length fnDecs) gen

    envBindingsFromFNDecs :: Env
    envBindingsFromFNDecs =
        M.fromList $ zip fnNames $ map TVar tVarNamesForFuncs

    completeEnv :: Env
    completeEnv = M.union envBindingsFromFNDecs envBindingsFromTCDecs

    in do

    (_, functionConstraints :: S.Set Constraint, gen'' :: NameGen) <-
        foldCG constraintsFNDec completeEnv gen' fnDecs

    (tiFuncs :: [FNDec], gen''' :: NameGen) <- tiDecsAsFuncs prog gen''

    (_, witnessConstraints :: S.Set Constraint, gen'''' :: NameGen) <-
        foldCG constraintsFNDec completeEnv gen''' tiFuncs

    (mainType :: Type, mainConstraints :: S.Set Constraint,
        gen''''' :: NameGen) <- constraintsExp completeEnv gen'''' (snd prog)

    return ( mainType
           , S.unions [ functionConstraints
                      , witnessConstraints
                      , mainConstraints
                      ]
           , gen'''''
           )

-- The given function expects a binding for its own name already in the Env
-- via a fresh type variable. constraintsProg adds this.
constraintsFNDec :: ConstraintGen FNDec ()
constraintsFNDec env gen (FNDec n ty args body) = let
    (argNames, gen') = genNNames (length args) gen
    envWithAll = M.unionWith (\_ x -> x) env $
        M.fromList $ zip args $ map TVar argNames 
    in do
    tyThis <- env ? n
    (tyBody, conBody, gen'') <- constraintsExp envWithAll gen' body
    return ((), S.insert (ty, tyThis) conBody, gen'')

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
