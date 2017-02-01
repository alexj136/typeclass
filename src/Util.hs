module Util where

import Data.List (intersperse)
import Control.Monad (liftM, ap)
import qualified Data.Map as M

-- Given a function for showing an a and a delimiter, show a list of as,
-- delimiting with said delimiter.
showAListWFn :: (a -> String) -> String -> [a] -> String
showAListWFn fn delim = concat . intersperse delim . map fn

-- Special case of showAListWFn where the type a is in the Show typeclass, so the
-- show function does not need to be passed explicitly.
showAList :: Show a => String -> [a] -> String
showAList = showAListWFn show

-- A more syntactically lightweight version of Data.Map.insert
(+=) :: Ord k => M.Map k v -> (k, v) -> M.Map k v
kvmap += (k, v) = M.insert k v kvmap

-- Like undefined, but more descriptive
notImplemented :: a
notImplemented = error "not yet implemented"

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

-- Map lookup in the Result monad as opposed to the Maybe monad
(?) :: (Ord k, Show k) => M.Map k v -> k -> Result v
kvmap ? k = case M.lookup k kvmap of
    Nothing -> Error $ "Map lookup failed for key: " ++ show k
    Just v  -> return v

-----------------------------------
-- Machinery for name generation --
-----------------------------------

type NameGen = Int

genNNames :: Int -> NameGen -> ([String], NameGen)
genNNames 0 gen = ([], gen)
genNNames n gen = let
    (rest   , genAfterRest) = genNNames (n - 1) gen
    (newName, genLast     ) = genName genAfterRest
    in
    (newName : rest, genLast)

genName :: NameGen -> (String, NameGen)
genName g = ("__" ++ alphaFromInteger g ++ "__", g + 1)
    where

    -- Generate a string of lowercase alphabetic characters from an integer by
    -- interpreting the integer in base 26 such that a 0 is a, 1 is b, etc.
    -- Generation is in reverse order so that the leading digit changes with
    -- addition of one, helping to distinguish variable names.
    alphaFromInteger :: Int -> String
    alphaFromInteger = map (\digit -> ['a'..'z'] !! digit) . digits 26

    -- Get the digits of a number in a given base, in reverse order
    digits :: Int -> Int -> [Int]
    digits base x | x == 0 = [0]
    digits base x | True   = ds base x where
        ds base x | x == 0 = []
                  | True   = (x `mod` base) : ds base (x `div` base)
