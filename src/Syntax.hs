{-# LANGUAGE FlexibleInstances #-}

module Syntax where

import Prelude hiding (showList)
import Data.List (intersperse)
import qualified Data.Set as S
import qualified Data.Map as M

import Util

type Name = String

-- A type for type class declarations: 'class X y where ... '
data TCDec = TCDec Name Name (M.Map Name Type) deriving (Eq, Ord)

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

-- A type to represent the typeclass information in a function declaration
type TypeClasses = S.Set (Name, Name)

-- A type for functions defined in a haskell style way with a type declaration
-- (which includes typeclasses) and a value declaration with argument names and
-- a body.
data FNDec = FNDec Name TypeClasses Type [Name] Exp
    deriving (Eq, Ord)

showTCs :: TypeClasses -> String
showTCs tcs
    | S.size tcs == 0 = ""
    | S.size tcs == 1 = (map (\(c, a) -> c ++ " " ++ a) (S.toList tcs)) !! 0
        ++ " => "
    | otherwise       = "(" ++ ((concat . intersperse ", ")
        (map (\(c, a) -> c ++ " " ++ a) (S.toList tcs))) ++ ") => "

instance Show FNDec where
    show (FNDec n tcs ty args body) =
        n ++ " :: " ++  showTCs tcs ++ show ty ++ "\n" ++
        n ++ " " ++ showList " " args ++ " = " ++ show body

-- Expression datatype - function bodies
data Exp
    = AppF Name [Exp]
    | Let Name {- = -} Exp {- in -} Exp
    | Lam Name Exp
    | AppL Exp Exp
    | Var Name
    | Const Int
    | Prod [Exp]
    | Index
    | Add
    | Sub
    deriving (Eq, Ord)

instance Show Exp where
    show e = case e of
        AppF n es  -> n ++ showList " " es
        Let n x y -> "let " ++ n ++ " = " ++ show x ++ " in " ++ show y
        Lam n x   -> "\\ " ++ n ++ " -> " ++ show x
        AppL f a  -> "(" ++ show f ++ ") " ++ show a
        Var n     -> n
        Const x   -> show x
        Prod es   -> "(" ++ showList ", " es ++ ")"
        Index     -> "index"
        Add       -> "(+)"
        Sub       -> "(-)"

-- Grammar for types
data Type
    = TProd [Type]
    | TFunc Type Type
    | TInt
    | TVar Name
    deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        TProd ts  -> "(" ++ showList ", " ts ++ ")"
        TFunc f a -> "(" ++ show f ++ " -> " ++ show a ++ ")"
        TInt      -> "Int"
        TVar a    -> a

-- in our language, a program is a list of declarations. A declaration is either
-- a function declaration, a type class declaration, or a type instance
-- declaration.
type Prog = [Dec]

data Dec = TC TCDec | TI TIDec | FN FNDec deriving (Eq, Ord)

instance Show Dec where
    show (TC tcdec) = show tcdec
    show (TI tidec) = show tidec
    show (FN fndec) = show fndec

instance Show [Dec] where
    show decs = showList "\n" decs
