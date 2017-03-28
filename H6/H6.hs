import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-----------------------------------

fibs2 :: [Integer]
fibs2 = reverse $ foldl (\l@(a:b:_) x -> (a+b) : l ) [1,0] [0..10000]

-------------------------------------------------------------------------

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a s) = a : streamToList s

instance Show a => Show (Stream a) where
    show s = intercalate "," $ map show (take 100 (streamToList s))

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