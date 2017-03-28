import Data.Char
import Data.Maybe
import Data.List

toDigits :: Integer -> [Integer]
toDigits i
    | i <= 0    = []
    | otherwise = map (toInteger . digitToInt) (show i)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = foldr doubleEv []
    where doubleEv i ls
            | (length ls) `mod` 2 /= 0  = (i * 2) : ls
            | otherwise                 = i : ls

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . (map toDigits)

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0

--------------------------------------------------

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n s d t = (hanoi (n - 1) s t d) ++ ((s, d) : (hanoi (n - 1) t d s))

--------------------------------------------------