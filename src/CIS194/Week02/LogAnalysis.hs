module CIS194.Week02.LogAnalysis where

import CIS194.Week02.Log

-- Exercise 01
parseMessage :: String -> LogMessage
parseMessage ('I':xs) = parseNonErrorMessage Info (words xs)
parseMessage ('W':xs) = parseNonErrorMessage Warning (words xs)
parseMessage ('E':xs) = parseErrorMessage (words xs)
parseMessage logEntry = Unknown logEntry

parseNonErrorMessage :: MessageType -> [String] -> LogMessage
parseNonErrorMessage messageType (ts:msg) =
  LogMessage messageType timestamp message
  where
    timestamp = read ts
    message   = unwords msg

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage (code:ts:msg) = LogMessage (Error severity) timestamp message
  where
    severity  = read code
    timestamp = read ts
    message   = unwords msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 02
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ t1 _) (Node left root@(LogMessage _ t2 _) right)
  | t1 <= t2  = Node (insert logMessage left) root right
  | otherwise = Node left root (insert logMessage right)
insert _ _ = undefined

-- Exercise 03
build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

-- Exercise 04
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left root right) = inOrder left ++ [root] ++ inOrder right

-- Exercise 05
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMessage . filter isSevereError . inOrder . build

extractMessage :: LogMessage -> String
extractMessage (Unknown message)        = message
extractMessage (LogMessage _ _ message) = message

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _) = severity >= 50
isSevereError _                                 = False
