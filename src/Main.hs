module Main where

import Data.Map (empty)

import Util
import TypeCheck
import Examples

main :: IO ()
main = let
    result = do
        (ty, plusProgConstraints, gen) <- constraintsProg empty 0 plusProg
        (unifyRes, gen') <- unify gen plusProgConstraints
        (ty', gen'') <- unifyRes gen' ty
        return ty'
    in do
        putStrLn $ case result of
            Success ty' -> show ty'
            Error   s   -> s
        return ()
