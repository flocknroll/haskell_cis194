{-# LANGUAGE FlexibleInstances #-}
import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-----------------------------------

fibs2 :: [Integer]
fibs2 = reverse $ foldl (\l@(a:b:_) x -> (a+b) : l ) [1,0] [0..50]

-------------------------------------------------------------------------

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

instance Show a => Show (Stream a) where
    show s = intercalate "," $ map show (take 50 (streamToList s))

-------------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x s) = Stream (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x s) (Stream y t) = Stream x (Stream y (interleaveStreams s t))

---------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

rFn :: Integer -> Integer
rFn n = head $ dropWhile (\x -> n `mod` (2^x) /= 0) [9,8..0] 

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap rFn (streamFromSeed (+2) 2))

rulerRec :: Integer -> Stream Integer
rulerRec 64 = streamRepeat 64
rulerRec n = interleaveStreams (streamRepeat n) (rulerRec (n+1))

ruler' :: Stream Integer
ruler' = rulerRec 0

---------------------------------------------------------------------------------------

x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n                    = Stream n (streamRepeat 0)
    negate s                         = streamMap negate s
    (+) (Stream a s) (Stream b t)    = Stream (a+b) (s + t)
    (*) (Stream a s) t@(Stream b t') = Stream   (a*b) -- a0 * b0
                                                (
                                                    streamMap (*a) t'   -- a0 * B'
                                                    +
                                                    s * t   -- A' * B
                                                )

instance Fractional (Stream Integer) where
    (/) s@(Stream a s') t@(Stream b t') = Stream    (a `div` b)
                                                    (
                                                        streamMap (\x -> x `div` b)
                                                        (s' - (s / t) * t')
                                                    )

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-------------------------------------------------------------------------------------------

data Matrix = Matrix Integer Integer Integer Integer
instance Num Matrix where
    (*) (Matrix a11 a12
                a21 a22)
        (Matrix b11 b12
                b21 b22) = Matrix   (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22)
                                    (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

getFn :: Matrix -> Integer
getFn (Matrix _ n _ _) = n

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = getFn $ (Matrix 1 1 1 0) ^ n