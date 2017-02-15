{-# LANGUAGE FlexibleInstances #-}  -- Allows 'instance Show [Dec] ...'

module Syntax where

import Data.List (intersperse)
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
    | TVar Name (S.Set Name)
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        TProd t u -> "(" ++ show t ++  ", " ++ show u ++ ")"
        TFunc f a -> "(" ++ show f ++ " -> " ++ show a ++ ")"
        TInt      -> "Int"
        TVar a cs | null cs   -> a
        TVar a cs | otherwise -> '(' : (concat $ intersperse ", " $
            map (\n -> n ++ " " ++ a) (S.toList cs)) ++ " => " ++ a ++ ")"
        TQuant n cs t | null cs   -> "∀ " ++ n ++ " . " ++ show t
        TQuant n cs t | otherwise -> "∀ " ++ n ++ " ∈ " ++
            showAList ", " (S.toList cs) ++ ". " ++ show t

tVar :: Name -> Type
tVar n = TVar n S.empty

tForAll :: [Name] -> Type -> Type
tForAll []     = id
tForAll (a:as) = TQuant a S.empty . tForAll as

tFuncN :: [Type] -> Type
tFuncN []     = error "tFuncN undefined for empty list"
tFuncN [t]    = t
tFuncN (t:ts) = TFunc t $ tFuncN ts

frees :: Type -> S.Set Name
frees t = case t of
    TProd t1 t2  -> frees t1 `S.union` frees t2
    TFunc t1 t2  -> frees t1 `S.union` frees t2
    TInt         -> S.empty
    TQuant x _ t -> S.delete x $ frees t
    TVar x _     -> S.singleton x

-- Replace occurences of 'x' by 'arg', within the expression 'body'. Alpha
-- converts as necessary to avoid erroneous capture of free variables in 'arg'.
typeSubst :: Name       -- x
          -> Type       -- arg
          -> Type       -- body
          -> NameGen    -- fresh name source
          -> (Type, NameGen)
typeSubst x arg body gen = case body of
    TProd t1 t2 -> let
        (t1', gen' ) = typeSubst x arg t1 gen
        (t2', gen'') = typeSubst x arg t2 gen'
        in (TProd t1' t2', gen'')
    TFunc t1 t2 -> let
        (t1', gen' ) = typeSubst x arg t1 gen
        (t2', gen'') = typeSubst x arg t2 gen'
        in (TFunc t1' t2', gen'')
    TInt -> (TInt, gen)
    TQuant y s ty | y `S.member` frees arg -> let
        (fresh, gen') = genName gen
        (ty', gen'')  = typeSubst x arg (dumbRename y fresh ty) gen'
        in (TQuant fresh s ty', gen'')
    TQuant y s ty | otherwise              -> let
        (ty', gen') = typeSubst x arg ty gen
        in (TQuant x s ty', gen')
    TVar y _ | x == y    -> (arg , gen)
    TVar y _ | otherwise -> (body, gen)

    where

    -- Rename all instances of one variable name with another, regardless of
    -- whether it is free or bound.
    dumbRename :: Name -> Name -> Type -> Type
    dumbRename from to body = case body of
        TProd t1 t2  -> TProd (dumbRename from to t1) (dumbRename from to t2)
        TFunc t1 t2  -> TFunc (dumbRename from to t1) (dumbRename from to t2)
        TInt         -> TInt
        TQuant x s t | x == from -> body
        TQuant x s t | otherwise -> TQuant x s (dumbRename from to t)
        TVar x cs    -> TVar (if x == from then to else x) cs

-- Alpha-equivalence on types
alpha :: Type -> Type -> Maybe (M.Map Name Name)
alpha t1 t2 = case (t1, t2) of
    (TProd a1 b1    , TProd a2 b2    ) ->
        (a1 `alpha` a2) `combine` (b1 `alpha` b2)
    (TFunc a1 b1    , TFunc a2 b2    ) ->
        (a1 `alpha` a2) `combine` (b1 `alpha` b2)
    (TInt           , TInt           ) -> Just $ M.empty
    (TQuant x1 s1 t1, TQuant x2 s2 t2) -> let t1AlphaT2 = t1 `alpha` t2 in
        if noClash t1AlphaT2 x1 x2 && s1 == s2 then t1AlphaT2 else Nothing
    (TVar x1 _      , TVar x2 _      ) -> return (M.singleton x1 x2)
    (_              , _              ) -> Nothing

    where

        combine :: Maybe (M.Map Name Name)
                -> Maybe (M.Map Name Name)
                -> Maybe (M.Map Name Name)
        combine s1 s2 = do
            m1 <- s1
            m2 <- s2
            let commonKeys = S.intersection (M.keysSet m1) (M.keysSet m2)
            let agreement  = S.map (\k -> m1 M.! k == m2 M.! k) commonKeys
            if S.member False agreement then
                return $ M.union m1 m2
            else
                Nothing

        noClash :: Maybe (M.Map Name Name) -> Name -> Name -> Bool
        noClash s x1 x2 = case s of { Nothing -> False; Just m ->
            (M.member    x1 m && m M.! x1 == x2        ) ||
            (M.notMember x1 m && notElem x2 (M.elems m))  }

-- in our language, a program is a list of declarations. A declaration is either
-- a function declaration, a type class declaration, or a type instance
-- declaration.
newtype Prog = Prog ([TCDec], [TIDec], [FNDec], Exp)

getTCDecs :: Prog -> [TCDec]
getTCDecs (Prog (c, _, _, _)) = c

getTIDecs :: Prog -> [TIDec]
getTIDecs (Prog (_, i, _, _)) = i

getFNDecs :: Prog -> [FNDec]
getFNDecs (Prog (_, _, f, _)) = f

getMain :: Prog -> Exp
getMain (Prog (_, _, _, m)) = m

tiDecsAsFuncs :: Prog -> Result [FNDec]
tiDecsAsFuncs prog =
    let classMap :: M.Map Name (Name, M.Map Name Type)
        classMap = M.fromList $ map (\(TCDec className binder functions) ->
            (className, (binder, functions))) $ getTCDecs prog
    in foldM (\done (TIDec className typeInClass witnesses) -> do
        (classBinder, classFnMap) <- classMap ? className
        if M.keys classFnMap /= M.keys witnesses then Error $
            "class-instance mismatch for " ++ className ++
            " and " ++ show typeInClass
        else foldM (\done (n, (args, body)) -> do
                tyF <- classFnMap ? n
                return $ FNDec n tyF args body : done
            ) done $ M.toList witnesses
        ) [] $ getTIDecs prog
