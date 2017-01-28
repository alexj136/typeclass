{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import qualified Data.Set as S
import qualified Data.Map as M

import Util

type Name = String

-- A type for type class declarations: 'class X y where ... '
data TCDec = TCDec
    { className  :: Name
    , memberName :: Name
    , functions  :: M.Map Name Type
    } deriving (Eq, Ord)

instance Show TCDec where
    show (TCDec cname tvar funcmap) =
        "class " ++ cname ++ " " ++ tvar ++ " where\n" ++
        (concat $ map (\(f, t) -> "    " ++ f ++ " :: " ++ show t ++ "\n") $
            M.toList funcmap)

-- A type for type instance declarations: 'instance X Int where ... '
data TIDec = TIDec Name Type (M.Map Name ([Name], Exp)) deriving (Eq, Ord)

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
