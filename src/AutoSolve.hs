module AutoSolve where

import Control.Monad
import Data.List
import Data.Ord
import Lib

allColorCombos :: [Code]
allColorCombos = replicateM numPins [minBound..maxBound]

initialGuess :: Code
initialGuess = [Red, Red, Orange, Orange]

eliminates :: [Code] -> Code -> (Int, Int) -> Int
eliminates candidates guess pegs = length $ filter (\c -> (checkBlack guess c, checkWhite guess c) /= pegs) candidates

score :: [Code] -> Code -> Int
score candidates guess = minimum [eliminates candidates guess (b,w) | b<-[0..numPins], w<-[0..numPins - b]]

minMax :: [Code] -> Code
minMax candidates = maximumBy (comparing (\g -> (score candidates g, g `elem` candidates))) allColorCombos

knuthOnce :: (Code -> Int, Code -> Int) -> (Code, [Code]) -> (Code, [Code])
knuthOnce (blackChecker, whiteChecker) (guess, candidates) =
        let candidatesMaybe = filter matchingPins candidates
            candidates' = if candidatesMaybe /= candidates then candidatesMaybe else tail candidatesMaybe
            guess' = minMax candidates'
        in (guess', candidates')
    
    where
        matchingPins c = (checkBlack guess c, checkWhite guess c) == (blackChecker guess, whiteChecker guess)

knuth' :: (Code -> Int, Code -> Int) -> [(Code,Int)]
knuth' checkers = let (guesses, done) = span ((/=1) . length . snd) $ iterate (knuthOnce checkers) (initialGuess, allColorCombos)
                 in map (fmap length) $ guesses ++ take 1 done

knuth :: (Code -> Int, Code -> Int) -> [Code]
knuth = map fst . knuth'

runKnuth' :: Code -> [(Code, Int)]
runKnuth' master = knuth' (flip checkBlack master, flip checkWhite master)

runKnuth :: Code -> [Code]
runKnuth = map fst . runKnuth'