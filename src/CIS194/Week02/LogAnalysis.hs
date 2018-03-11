{-# OPTIONS_GHC -Wall #-}
module CIS194.Week02.LogAnalysis where

import           CIS194.Week02.Log

readHead :: [String] -> Int
readHead = read . head

concatenateTail :: [String] -> String
concatenateTail = unwords . tail

parseMessage :: String -> LogMessage
parseMessage (x:xs)
    | x == 'E' && (length parts) >= 2   = LogMessage (Error (readHead  parts))
                                                ((readHead . tail) parts)
                                                (unwords (drop 2 parts))
    | x == 'W' && (length parts) >= 3   = LogMessage Warning (readHead parts) (concatenateTail parts)
    | x == 'I' && (length parts) >= 2   = LogMessage Info (readHead parts) (concatenateTail parts)
    | otherwise             = Unknown (x:xs)
    where parts = words xs
parseMessage xs = Unknown xs


parseLines :: [String] -> [LogMessage]
parseLines []       = parseMessage [] : []
parseLines (x:[])   = parseMessage x : []
parseLines (x:xs)   = parseMessage x : parseLines xs

parse :: String -> [LogMessage]
parse xs =  parseLines (lines xs)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree    = tree
insert msg Leaf            = Node Leaf msg Leaf
insert msg@(LogMessage _ stamp _) (Node leftTree currentMessage@(LogMessage _ currentStamp _) rightTree)
    | stamp > currentStamp = Node leftTree currentMessage (insert msg rightTree)
    | otherwise            = Node (insert msg leftTree) currentMessage  rightTree
insert _ tree              = tree -- show throw an exception here, idk how to in Haskell yet sooo..

buildFromTree :: MessageTree -> [LogMessage]  -> MessageTree
buildFromTree tree []   = tree
buildFromTree tree (x:xs) = buildFromTree (insert x tree) xs

build :: [LogMessage] -> MessageTree
build = buildFromTree Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree message rightTree) =
    (inOrder leftTree) ++ [message] ++ (inOrder rightTree)

orderMessages :: [LogMessage] -> [LogMessage]
orderMessages = inOrder . build

moreThan :: Int -> LogMessage -> Bool
moreThan min_severity (LogMessage (Error severity) _ _) = severity >= min_severity
moreThan _ _ = False

moreThan50 :: LogMessage -> Bool
moreThan50 = moreThan 50

filterMessages :: [LogMessage] -> [LogMessage]
filterMessages = filter moreThan50

onlyErrors :: [LogMessage] -> [LogMessage]
onlyErrors [] = []
onlyErrors (x:xs) =
    case x of
        msg@(LogMessage (Error _) _ _)  -> msg : onlyErrors xs
        _                               -> onlyErrors xs

extractMessage :: LogMessage -> String
extractMessage (LogMessage _ _ msg) = msg
extractMessage _ = ""

extractMessages :: [LogMessage] -> [String]
extractMessages = map extractMessage

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMessages . filterMessages . onlyErrors . orderMessages


























