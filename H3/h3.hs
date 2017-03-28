module Golf where

import Data.List

s :: [a] -> Int -> [[a]]
s _ 0 = []
s l n = s l (n-1) ++ [foldr (\x a -> l !! (x-1) : a) [] [n,2*n..length l]]

skips :: [a] -> [[a]]
skips l = s l (length l)

---------------------------------------------------------------------------

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:x)
                | a < b && b > c = b : l
                | True           = l
                where l = localMaxima (b:c:x)
localMaxima _ = []

---------------------------------------------------------------------------

c :: [Integer] -> Integer -> [Int]
c _ (-1)    = []
c l n       = c l (n-1) ++ [length (filter (n==) l)]

d :: [Int] -> Int -> String
d _ 0 = "==========\n0123456789\n"
d t n = map (\x -> if x >= n then '*' else ' ') t ++ "\n" ++ d t (n-1)

histogram :: [Integer] -> String
histogram n = let l = (c n 9) in d l (maximum l) 
