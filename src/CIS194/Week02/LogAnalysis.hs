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
  LogMessage messageType (read ts) (unwords msg)

parseErrorMessage :: [String] -> LogMessage
parseErrorMessage (code:ts:msg) =
  LogMessage (Error (read code)) (read ts) (unwords msg)

parse :: String -> [LogMessage]
parse logEntries = map parseMessage (lines logEntries)

-- Exercise 02
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node left root right)
  | timestamp logMessage <= timestamp root = Node (insert logMessage left) root right
  | otherwise                              = Node left root (insert logMessage right)

-- AFC: Pattern match against Unknown
timestamp :: LogMessage -> TimeStamp
timestamp (LogMessage Info timestamp _)      = timestamp
timestamp (LogMessage Warning timestamp _)   = timestamp
timestamp (LogMessage (Error _) timestamp _) = timestamp
