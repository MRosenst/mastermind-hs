module Main where

import Lib
import System.Random

rounds :: Int
rounds = 10

genCode :: IO Code
genCode = do
  gen <- getStdGen
  let randColors = randoms gen
  return $ take numPins randColors

main :: IO ()
main = genCode >>= nRounds rounds
