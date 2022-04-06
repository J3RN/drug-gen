module Lib
  ( createMatrix
  , Matrix
  ) where

import Control.Monad
import Data.Ratio
import qualified Data.Map as Map

type SumMatrix = Map.Map Char (Map.Map Char Int)
type Matrix = Map.Map Char (Map.Map Char Rational)

createMatrix :: [String] -> Matrix
createMatrix drugNames =
  let sumMatrix = foldl updateForWord (Map.fromList []) drugNames
  in rationalize sumMatrix

updateForWord :: SumMatrix -> String -> SumMatrix
updateForWord matrix word =
  foldl updateAssoc matrix (charPairs word)

updateAssoc :: SumMatrix -> (Char, Char) -> SumMatrix
updateAssoc m (l1,l2) = Map.alter (incAssoc l2) l1 m
  where incAssoc l2 Nothing = Just (Map.fromList [(l2, 1)])
        incAssoc l2 (Just m2) = Just (updateCount l2 m2)

updateCount :: Char -> Map.Map Char Int -> Map.Map Char Int
updateCount = Map.alter inc
  where inc Nothing = Just 1
        inc (Just n) = Just (n + 1)

-- Wraps the string in null bytes first so we have special pairs for the first
-- and last characters of a string
charPairs :: String -> [(Char, Char)]
charPairs = (liftM2 zip init tail) . (++ "\0") . ((:) '\0')

rationalize :: SumMatrix -> Matrix
rationalize = Map.map rationalizeAssoc

rationalizeAssoc :: (Map.Map Char Int) -> (Map.Map Char Rational)
rationalizeAssoc m =
  let total = sum . Map.elems $ m :: Int
  in Map.map ((% (toInteger total)) . toInteger) m
