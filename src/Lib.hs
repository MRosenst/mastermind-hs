module Lib
     
     where

import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.List
import Data.Maybe
import System.Random
import Text.Printf

data Color = Red | Orange | Yellow | Green | Blue | Purple
  deriving (Eq, Enum, Show, Read, Ord, Bounded)

instance Random Color where
  randomR = first toEnum .: randomR . bimap fromEnum fromEnum
  random = randomR (minBound, maxBound)

type Code = [Color]

(.:) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
(.:) = (.) . (.)
infixr 1 .:

numPins :: Int
numPins = 4

-- |Calculates the number of black pins. Commutative.
checkBlack :: Code -> Code -> Int
checkBlack = length . filter id .: zipWith (==)

-- |Calculates the number of white pins. Not commutative - 'checkWhite guess master'
checkWhite :: Code -> Code -> Int
checkWhite guess master = length $ master' `intersect` guess'
  where
    -- remove matching colors with matching positions
    (guess', master') = foldl go ([], []) (zip guess master)
    go (gs, ms) (g, m) = if g == m then (gs, ms) else (g:gs, m:ms)

letterToColor :: Char -> Maybe Color
letterToColor c = case c of
  'r' -> Just Red
  'o' -> Just Orange
  'y' -> Just Yellow
  'g' -> Just Green
  'b' -> Just Blue
  'p' -> Just Purple
  _   -> Nothing

restart :: Code -> IO Bool
restart master = putStrLn "Invalid guess!" >> singleRound master

singleRound :: Code -> IO Bool
singleRound master = do
  putStrLn "Enter a guess:"
  guessMay <- fmap (traverse letterToColor . filter isAlpha) getLine
  case guessMay of
    Nothing -> restart master
    Just guess ->
      if length guess /= numPins
        then restart master
        else do
          let black = checkBlack guess master
              white = checkWhite guess master
          printf "White pins: %d\nBlack pins: %d\n\n" white black

          if black == numPins
            then putStrLn "You Win!" >> return True
            else return False

untilM :: Monad m => (a -> Bool) -> [m a] -> m ()
untilM _ [] = return ()
untilM p (x:xs) = do
  y <- x
  unless (p y) $ untilM p xs

nRounds :: Int -> Code -> IO ()
nRounds n master = do
  let allRounds = replicate n (singleRound master)
  untilM id allRounds
