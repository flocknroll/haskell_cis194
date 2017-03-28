{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import qualified Data.Map as M

eval :: ExprT -> Integer
eval (Lit n)   = n
eval (Mul x y) = eval x * eval y
eval (Add x y) = eval x + eval y

-------------------------------------

evalStr :: String -> Maybe Integer
evalStr s = fmap eval (parseExp Lit Add Mul s)

-------------------------------------
class Expr a where
    lit :: Integer -> a
    mul :: a -> a -> a
    add :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    mul = Mul
    add = Add

--------------------------------------

instance Expr Integer where
    lit = id
    mul = (*)
    add = (+)

instance Expr Bool where
    lit n = n > 0
    mul = (&&)
    add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    mul (MinMax x) (MinMax y) = lit $ if x < y then x else y
    add (MinMax x) (MinMax y) = lit $ if x > y then x else y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7
    mul (Mod7 x) (Mod7 y) = lit $ (x * y) `mod` 7
    add (Mod7 x) (Mod7 y) = lit $ (x + y) `mod` 7
    
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4) + 55"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

-----------------------------------------------------------------------------

class HasVars a where
    var :: String -> a

data VarExprT = VLit Integer
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = VLit
    mul = VMul
    add = VAdd
instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var k m = M.lookup k m

instance Expr (M.Map String Integer -> Maybe Integer) where
   lit i _   = Just i
   mul f g m = do
                rf <- f m
                rg <- g m
                return $ rf * rg
   add f g m = (+) <$> (f m) <*> (g m)

withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs exp = exp $ M.fromList vs