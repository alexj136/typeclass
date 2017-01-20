module Util where

import Data.List (intersperse)
import Control.Monad (liftM, ap)

-- Given a function for showing an a and a delimiter, show a list of as,
-- delimiting with said delimiter.
showAListWFn :: (a -> String) -> String -> [a] -> String
showAListWFn fn delim = concat . intersperse delim . map fn

-- Special case of showAListWFn where the type a is in the Show typeclass, so the
-- show function does not need to be passed explicitly.
showAList :: Show a => String -> [a] -> String
showAList = showAListWFn show

------------------
-- Result Monad --
------------------

data Result a = Error String | Success a deriving (Show, Eq, Ord)

instance Functor Result where
    fmap = liftM

instance Applicative Result where
    pure  = return
    (<*>) = ap

instance Monad Result where
    (Error   s) >>= f = Error s
    (Success a) >>= f = f a
    return = Success
