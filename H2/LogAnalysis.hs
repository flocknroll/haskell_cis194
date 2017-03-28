{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

makeMessage :: MessageType -> String -> LogMessage
makeMessage t m = LogMessage t (read . head . words $ m) (unwords . tail . words $ m)

parseMessage :: String -> LogMessage
parseMessage m = case m of
                    ('E':' ':ms) -> makeMessage (Error (read . head . words $ ms)) (unwords . tail . words $ ms)
                    ('I':' ':ms) -> makeMessage Info ms
                    ('W':' ':ms) -> makeMessage Warning ms
                    _        -> Unknown m

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t    = t
insert msg Leaf         = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node tl msg'@(LogMessage _ ts' _) tr)
            | ts <= ts' = Node (insert msg tl) msg' tr
            | ts > ts'  = Node tl msg' (insert msg tr)

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (m:ms) = insert m (build ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l n r) = inOrder l ++ (n : inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong m = let   filter50 l = case l of
                                  LogMessage (Error s) _ _ -> if s > 50 then True else False
                                  _ -> False
                        getMsg (LogMessage _ _ msg) = msg
                        logs = inOrder . build . filter (\x -> filter50 x) $ m
                  in map getMsg logs