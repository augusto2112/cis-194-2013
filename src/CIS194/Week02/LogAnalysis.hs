module CIS194.Week02.LogAnalysis where

import CIS194.Week02.Log

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
