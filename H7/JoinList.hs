{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                    | Single m a
                    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty l = l
(+++) l Empty = l
(+++) l l'    = Append (mappend (tag l) (tag l')) l l'

----------------------------------------------------------------------

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

getJSize :: (Sized b, Monoid b) => JoinList b a -> Int
getJSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ i _ | i < 0    = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ l l')
            | i < sl       = indexJ i l
            | i < sl + sl' = indexJ (i-sl) l'
            | otherwise    = Nothing
            where sl  = getJSize l
                  sl' = getJSize l'

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i l
        | i <= 0          = l
        | i >= getJSize l = Empty
dropJ i (Append _ l l')   = (dropJ i l) +++ l'

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i l
        | i <= 0          = Empty
        | i >= getJSize l = l
takeJ i (Append _ l l')   = takeJ i l

--------------------------------------------------

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-----------------------------------------------------

instance Buffer (JoinList (Score, Size) String) where
    toString Empty           = []
    toString (Single _ s)    = s
    toString (Append _ l l') = toString l ++ "\n" ++ toString l'
    fromString s = foldl (\b l -> b +++ (Single (scoreString l, 1) l)) Empty (lines s)
    line = indexJ
    replaceLine i s b = (takeJ i b) +++ (fromString s) +++ (dropJ (i+1) b)
    numLines = getJSize
    value = getScore . fst . tag

initBuffer :: JoinList (Score, Size) String
initBuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

main = runEditor editor initBuffer 