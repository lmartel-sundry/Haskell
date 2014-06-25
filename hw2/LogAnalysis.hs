{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

import Test

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = parseWords $ words s

parseWords :: [String] -> LogMessage
parseWords ("I":t:ws) = LogMessage Info (read t) (unwords ws)
parseWords ("W":t:ws) = LogMessage Warning (read t) (unwords ws)
parseWords ("E":n:t:ws) = LogMessage (Error $ read n) (read t) (unwords ws)
parseWords xs = Unknown $ unwords xs

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ tIns _) (Node left root@(LogMessage _ tRoot _) right)
    | tIns < tRoot = Node (insert m left) root right
    | otherwise    = Node left root (insert m right)

insert _ (Node _ (Unknown _) _) = Leaf -- This shouldn't happen but I don't know how to errors yet

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ m : inOrder right 

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter isSevere . inOrder . build

isSevere :: LogMessage -> Bool
isSevere (LogMessage (Error n) _ _) = n >= 50
isSevere _ = False

message :: LogMessage -> String
message (LogMessage _ _ m) = m
message (Unknown m) = m

-- Begin tests
main :: IO()
main = do
    assert (parseMessage "E 2 562 help help") (LogMessage (Error 2) 562 "help help")
    assert (parseMessage "I 29 la la la") (LogMessage Info 29 "la la la")
    assert (parseMessage "This is not in the right format") (Unknown "This is not in the right format")

-- Interactive tests:
-- testParse parse 10 "error.log"
-- testWhatWentWrong parse whatWentWrong "sample.log"

