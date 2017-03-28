{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import StackVM
import Parser

class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr Program where
    lit n = [PushI n]
    mul a b = a ++ b ++ [Mul]
    add a b = a ++ b ++ [Add]

compile :: String -> Maybe Program
compile = parseExp lit add mul

getProgram :: Maybe Program -> Program
getProgram Nothing  = []
getProgram (Just p) = p

testStack :: String -> Either String StackVal
testStack = stackVM . getProgram . compile