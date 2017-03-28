module Scrabble where

import Data.Char

newtype Score = Score Int deriving (Show)

instance Monoid Score where
    mempty = Score 0
    mappend (Score s) (Score s') = Score (s + s')

score :: Char -> Score
score c
    | l `elem` "aeilnorstu" = Score 1
    | l `elem` "dg"         = Score 2
    | l `elem` "bcmp"       = Score 3
    | l `elem` "fhvwy"      = Score 4
    | l `elem` "k"          = Score 5
    | l `elem` "jx"         = Score 8
    | l `elem` "qz"         = Score 10
    | otherwise             = Score 0
    where l = toLower c

scoreString :: String -> Score
scoreString = foldr (\x a -> mappend (score x) a) mempty

getScore :: Score -> Int
getScore (Score s) = s