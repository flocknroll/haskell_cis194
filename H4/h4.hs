import Data.Bool

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
        | even x    = (x - 2) * fun1 xs
        | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' l = product . map (\x -> x-2) . filter even $ l

--------------------------------------------------------------------

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | even n    = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

calcNext :: Integer -> Integer
calcNext n = if even n
                then n `div` 2
                else 3 * n + 1

fun2' :: Integer -> Integer
fun2' n = sum . filter even . takeWhile (>1) . iterate calcNext $ n

--------------------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

insert :: Tree a -> a -> Tree a
insert Leaf x = Node 0 Leaf x Leaf
insert (Node _ l v r) x
                | hl < hr   = let tl = insert l x
                                  nh = (if getHeight tl > hr then getHeight tl else hr) + 1
                                in Node nh tl v r
                | otherwise = let tr = insert r x
                                  nh = (if getHeight tr > hl then getHeight tr else hl) + 1
                                in Node nh l v tr
                where   hl = getHeight l
                        hr = getHeight r

foldTree :: [a] -> Tree a
foldTree = foldl insert Leaf

-----------------------------------------------------------------------------------------

xor :: [Bool] -> Bool
xor = foldl (\x a -> (x && not a) || (not x && a)) False

-----------------------------------------------------------------------------------------

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x a -> f x : a) []

-----------------------------------------------------------------------------------------

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\a b -> f b a) base (reverse xs)

-----------------------------------------------------------------------------------------

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

excl :: Integer -> [Integer]
excl n = let calc i j = i + j + 2 * i * j
                in map (\(i,j) -> calc i j) . filter (\(i,j) -> calc i j <= n) $ cartProd [1..floor (sqrt (fromIntegral n / 2))] [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = let ex = excl n in
                    map (\x -> 2 * x + 1) . filter (\i -> not (i `elem` ex)) $ [1..n]