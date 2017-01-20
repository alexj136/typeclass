module Util where

import Prelude hiding (showList)
import Data.List (intersperse)

-- Given a function for showing an a and a delimiter, show a list of as,
-- delimiting with said delimiter.
showListWFn :: (a -> String) -> String -> [a] -> String
showListWFn fn delim = concat . intersperse delim . map fn

-- Special case of showListWFn where the type a is in the Show typeclass, so the
-- show function does not need to be passed explicitly.
showList :: Show a => String -> [a] -> String
showList = showListWFn show
