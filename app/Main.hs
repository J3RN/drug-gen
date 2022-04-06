module Main where

import Lib
import System.Random
import Control.Monad.Random
import qualified Data.Map as Map

main :: IO ()
main = do
  rawInput <- readFile "database.txt"
  let matrix = createMatrix . lines $ rawInput
  drug <- generateDrug matrix '\0'
  putStrLn drug

generateDrug :: Matrix -> Char -> IO String
generateDrug m a =
  case Map.lookup a m of
    Nothing -> return []
    Just mappings ->
      do
        next <- weighted (Map.toList mappings)
        if next == '\0'
          then return []
          else mappend (return [next]) (generateDrug m next)
