module Examples where

import qualified Data.Map as M

import Syntax

-- Declares typeclass 'Plusable' with a single function 'add'. Implementation
-- for ints is just the builtin add function. Implementation for Prod Int Int is
-- pairwise: add (a, b) (c, d) = (Add a c, Add b d). The main expression is
-- add (10, 5) (add 2 6, 4) which should yield (18, 9).
plusProg :: Prog
plusProg =
    (
        [ TC $ TCDec "Plusable" "a" $ M.fromList
            [("add", tFuncN [TVar "a", TVar "a", TVar"a"])]
        , TI $ TIDec "Plusable" TInt $ M.fromList
            [("add", ([], Add))]
        , TI $ TIDec "Plusable" (TProd TInt TInt) $ M.fromList
            [("add", (["p1", "p2"],
                Let "p1l" (App Fst (Var "p1")) (
                Let "p1r" (App Snd (Var "p1")) (
                Let "p2l" (App Fst (Var "p2")) (
                Let "p2r" (App Snd (Var "p2")) (
                    Prod (
                        App (App Add (Var "p1l")) (Var "p2l")
                    ) (
                        App (App Add (Var "p1r")) (Var "p2r")
                    )
            ))))))]
        ]
    , App (App (Var "add") (Prod (Const 10) (Const 5))) (Prod (
          App (App (Var "add") (Const 2)) (Const 6)) (Const 4))
    )
