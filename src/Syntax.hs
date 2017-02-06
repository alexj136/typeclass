{-# LANGUAGE FlexibleInstances #-}  -- Allows 'instance Show [Dec] ...'

module Syntax where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import Control.Monad (foldM)
import qualified Data.Set as S
import qualified Data.Map as M

import Util

-- A type for type class declarations: 'class X y where ... '
data TCDec = TCDec
    { classNameTC :: Name
    , memberName  :: Name
    , functions   :: M.Map Name Type
    } deriving (Eq, Ord)

instance Show TCDec where
    show (TCDec cname tvar funcmap) =
        "class " ++ cname ++ " " ++ tvar ++ " where\n" ++
        (concat $ map (\(f, t) -> "    " ++ f ++ " :: " ++ show t ++ "\n") $
            M.toList funcmap)

-- A type for type instance declarations: 'instance X Int where ... '
data TIDec = TIDec 
    { classNameTI :: Name
    , memberType  :: Type
    , functionMap :: M.Map Name ([Name], Exp)
    } deriving (Eq, Ord)

instance Show TIDec where
    show (TIDec cname ty funcmap) =
        "instance " ++ cname ++ " " ++ show ty ++ " where\n" ++
        (concat $ map (\(f, (ns, e)) -> "    " ++
            (concat (intersperse " " (f : ns))) ++ " = " ++ show e ++ "\n") $
                M.toList funcmap)

-- A type for functions defined in a haskell style way with a type declaration
-- (which includes typeclasses) and a value declaration with argument names and
-- a body.
data FNDec = FNDec
    { fnName   :: Name
    , fnType   :: Type 
    , argNames :: [Name]
    , body     :: Exp
    } deriving (Eq, Ord)

instance Show FNDec where
    show (FNDec n ty args body) =
        n ++ " :: " ++ show ty ++ "\n" ++
        n ++ " " ++ showAList " " args ++ " = " ++ show body

-- Expression datatype - function bodies
data Exp
    = App Exp Exp
    | Let Name {- = -} Exp {- in -} Exp
    | Lam Name Exp
    | Var Name
    | Const Int
    | Prod Exp Exp
    | Fst
    | Snd
    | Add
    | Sub
    deriving (Eq, Ord)

instance Show Exp where
    show e = case e of
        App f a  -> "(" ++ show f ++ ") " ++ show a
        Let n x y -> "let " ++ n ++ " = " ++ show x ++ " in " ++ show y
        Lam n x   -> "\\ " ++ n ++ " -> " ++ show x
        Var n     -> n
        Const x   -> show x
        Prod e f  -> "(" ++ show e ++ ", " ++ show f ++ ")"
        Fst       -> "fst"
        Snd       -> "snd"
        Add       -> "(+)"
        Sub       -> "(-)"

-- Grammar for types
data Type
    = TProd Type Type
    | TFunc Type Type
    | TInt
    | TQuant Name (S.Set Name) Type
    | TVar Name
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        TProd t u -> "(" ++ show t ++  ", " ++ show u ++ ")"
        TFunc f a -> "(" ++ show f ++ " -> " ++ show a ++ ")"
        TInt      -> "Int"
        TVar a    -> a
        TQuant n cs t | null cs   -> "∀ " ++ n ++ " . " ++ show t
        TQuant n cs t | otherwise -> "∀ " ++ n ++ " ∈ " ++
            showAList ", " (S.toList cs) ++ ". " ++ show t

tForAll :: [Name] -> Type -> Type
tForAll []     = id
tForAll (a:as) = TQuant a S.empty . tForAll as

tFuncN :: [Type] -> Type
tFuncN [t]    = t
tFuncN (t:ts) = TFunc t $ tFuncN ts

frees :: Type -> S.Set Name
frees t = case t of
    TProd t1 t2  -> frees t1 `S.union` frees t2
    TFunc t1 t2  -> frees t1 `S.union` frees t2
    TInt         -> S.empty
    TQuant x _ t -> S.delete x $ frees t
    TVar x       -> S.singleton x

-- Replace occurences of 'x' by 'arg', within the expression 'body'. Alpha
-- converts as necessary to avoid erroneous capture of free variables in 'arg'.
typeSubst :: Name    -- x
          -> Type    -- arg
          -> NameGen -- fresh name source
          -> Type    -- body
          -> Result (Type, NameGen)
typeSubst x arg gen body = case body of
    TProd t1 t2 -> do
        (t1', gen' ) <- typeSubst x arg gen  t1
        (t2', gen'') <- typeSubst x arg gen' t2
        return (TProd t1' t2', gen'')
    TFunc t1 t2 -> do
        (t1', gen' ) <- typeSubst x arg gen  t1
        (t2', gen'') <- typeSubst x arg gen' t2
        return (TFunc t1' t2', gen'')
    TInt -> return (TInt, gen)
    TQuant x s ty | x `S.notMember` frees ty -> do
        (ty', gen') <- typeSubst x arg gen ty
        return (TQuant x s ty', gen')
    TQuant x s ty | x `S.member` frees ty -> do
        let (fresh, gen') = genName gen
        (ty', gen'') <- typeSubst x arg gen' (dumbRename x fresh ty)
        return (TQuant fresh s ty', gen'')
    TVar y | x == y -> return (arg, gen)

    where

    -- rename all free instances of one variable name with another
    dumbRename :: Name -> Name -> Type -> Type
    dumbRename from to body = case body of
        TProd t1 t2  -> TProd (dumbRename from to t1) (dumbRename from to t2)
        TFunc t1 t2  -> TFunc (dumbRename from to t1) (dumbRename from to t2)
        TInt         -> TInt
        TQuant x s t | x == from -> body
        TQuant x s t | x /= from -> TQuant x s (dumbRename from to t)
        TVar x       -> TVar $ if x == from then to else x

-- in our language, a program is a list of declarations. A declaration is either
-- a function declaration, a type class declaration, or a type instance
-- declaration.
type Prog = ([Dec], Exp)

data Dec = TC TCDec | TI TIDec | FN FNDec deriving (Eq, Ord)

getTCDecs :: Prog -> [TCDec]
getTCDecs = getDecs (\d -> case d of { TC c -> Just c ; _ -> Nothing })

getTIDecs :: Prog -> [TIDec]
getTIDecs = getDecs (\d -> case d of { TI i -> Just i ; _ -> Nothing })

getFNDecs :: Prog -> [FNDec]
getFNDecs = getDecs (\d -> case d of { FN f -> Just f ; _ -> Nothing })

getDecs :: (Dec -> Maybe a) -> (Prog -> [a])
getDecs decSelector = catMaybes . map decSelector . fst

instance Show Dec where
    show (TC tcdec) = show tcdec
    show (TI tidec) = show tidec
    show (FN fndec) = show fndec

instance Show [Dec] where
    show decs = showAList "\n" decs

tiDecsAsFuncs :: Prog -> NameGen -> Result ([FNDec], NameGen)
tiDecsAsFuncs prog gen =
    let classMap :: M.Map Name (Name, M.Map Name Type)
        classMap = M.fromList $ map (\(TCDec className binder functions) ->
            (className, (binder, functions))) $ getTCDecs prog
    in foldM (\(done, curGen) (TIDec className typeInClass witnesses) -> do
        (classBinder, classFnMap) <- classMap ? className
        if M.keys classFnMap /= M.keys witnesses then Error $
            "class-instance mismatch for " ++ className ++
            " and " ++ show typeInClass
        else foldM (\(done, curGen) (n, (args, body)) -> do
            tyF <- classFnMap ? n
            (newTy, newGen) <- typeSubst classBinder typeInClass curGen tyF
            return $ (FNDec n newTy args body : done, newGen)
            ) (done, curGen) $ M.toList witnesses
        ) ([], gen) $ getTIDecs prog
