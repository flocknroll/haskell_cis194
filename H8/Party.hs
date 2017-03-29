module Party where

import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (empFun e + f)

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend (GL l f) (GL l' f') = GL (l ++ l') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ f) l'@(GL _ f') = if f > f' then l else l'

---------------------------------------------------------------

treeFold :: b -> ([b] -> a -> b) -> Tree a -> b
treeFold e f (Node val subTree) = let subFold = treeFold e f
                                    in f (map subFold subTree) val

----------------------------------------------------------------------