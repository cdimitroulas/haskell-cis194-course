module WeekTwo.LogAnalysis where

import WeekTwo.Log

parseMessage :: String -> LogMessage
parseMessage x =
  let wordList = words x
   in case wordList of
        ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
        ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
        ("E":code:ts:msg) ->
          LogMessage (Error (read code :: Int)) (read ts) (unwords msg)
        _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse x = map parseMessage (lines x)

-- inserts a new LogMessage into an existing MessageTree, producing a new MessageTree
insert :: LogMessage -> MessageTree -> MessageTree
insert logMsg@LogMessage {} Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ ts _) (Node leftTree logMsg2@(LogMessage _ ts2 _) rightTree)
  | ts < ts2 = Node (insert logMsg leftTree) logMsg2 rightTree
  | otherwise = Node leftTree logMsg2 (insert logMsg rightTree)
insert _ tree = tree

-- builds a MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- takes a sorted MessageTree and produces a list of all the
-- LogMessages it contains, sorted by timestamp from smallest to biggest.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error x) _ _) = x >= 50
isSevereError _ = False

getLogMessage :: LogMessage -> String
getLogMessage (LogMessage _ _ msg) = msg
getLogMessage _ = ""

-- takes an unsorted list of LogMessages, and returns a list of the
-- messages corresponding to any errors with a severity of 50 or greater,
-- sorted by timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map getLogMessage (filter isSevereError ((inOrder . build) x))
