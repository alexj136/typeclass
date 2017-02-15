module Interpreter where

import qualified Data.Map as M

import Util
import Syntax

type ExecEnv = M.Map Name Exp

interpretProg :: Prog -> Exp
interpretProg = undefined

interpretExp :: ExecEnv -> Exp -> Exp
interpretExp env exp = case exp of
    App func arg   -> undefined
    Let n def body -> undefined
    Lam n body     -> undefined
    Var n          -> undefined
    Const x        -> undefined
    Prod e1 e2     -> undefined
    Fst            -> undefined
    Snd            -> undefined
    Add            -> undefined
    Sub            -> undefined
