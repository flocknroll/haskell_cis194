module Party where

import Data.Tree
import Data.List
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL l f) = GL (e:l) (empFun e + f)

instance Monoid GuestList where
    mempty  = GL [] 0
    mappend (GL l f) (GL l' f') = GL (l ++ l') (f + f')

moreFun :: GuestList -> GuestList -> GuestList
moreFun l@(GL _ f) l'@(GL _ f') = if f > f' then l else l'

---------------------------------------------------------------

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold e f (Node val subTree) = let subFold = treeFold e f
                                    in f val (map subFold subTree)

----------------------------------------------------------------------

nextLevel :: Employee -> [(GuestList, GuestList)]
                -> (GuestList, GuestList)
nextLevel e l = let maxFunBoss  = foldl (\a (_, g)  -> a `mappend` g)                mempty l
                    maxFunOther = foldl (\a (g, g') -> (a `mappend` (moreFun g g'))) mempty l
                in (glCons e maxFunBoss, maxFunOther)

--------------------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun t = let (g, g') = treeFold mempty nextLevel t
            in moreFun g g'

---------------------------------------------------------------

getNames :: [Employee] -> String
getNames = unlines . sort . map empName

showGuestList :: GuestList -> String
showGuestList (GL e f) = "Total fun: " ++ show f ++ "\n" ++ getNames e

main :: IO ()
main = do
        file <- readFile "company.txt"
        let list = showGuestList . maxFun . read $ file
        putStrLn list