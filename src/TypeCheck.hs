module TypeCheck where

import qualified Data.Set as S

import Util
import Syntax

type Constraint = (Type, Type)

genConstraints :: Prog -> Result (S.Set Constraint)
genConstraints = undefined

unify :: S.Set Constraint -> Result (Type -> Type)
unify = undefined
