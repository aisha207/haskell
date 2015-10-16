{-# OPTIONS_GHC -Wall #-}
module LogParser where

import Log

-- Parse a single log message. Each log message starts with I (info, W (warn),
-- E (error). E messages are followed by a numerical severity [0, 100]. All
-- messages have the format I/W/E [severity] timestamp(int) message
parseMessage :: String -> LogMessage
parseMessage msg 
  | t == "E" = parseRemainder (Error (read (msgWords !! 1) :: Int)) (drop 2 msgWords)
  | t == "W" = parseRemainder Warning (drop 1 msgWords)
  | t == "I" = parseRemainder Info (drop 1 msgWords)
  | otherwise = Unknown msg
    where msgWords = words msg
          t = head msgWords

-- Finish parsing the message
parseRemainder :: MessageType -> [String] -> LogMessage
parseRemainder mType msgWords = LogMessage mType timestamp text
                                where timestamp = read (head msgWords) :: Int
                                      text = unwords (tail msgWords)

-- Parse an entire file
parse :: String -> [LogMessage]
parse msgs = [parseMessage m | m <- lines msgs]


-- Insert the LogMessage into an existing binary tree
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ timestamp _) (Node lTree cur@(LogMessage _ current _) rTree)
  | timestamp > current = Node lTree cur (insert msg rTree)
  | timestamp <= current = Node (insert msg lTree) cur rTree
insert _ _ = error "Malformed data"


build :: [LogMessage] -> MessageTree
build msgs = buildAcc msgs Leaf
  where buildAcc [] tree = tree
        buildAcc (x:xs) tree = buildAcc xs $ insert x tree

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = (inOrder lTree) ++ [msg] ++ (inOrder rTree)

-- Example usage so far
-- let a = testParse parse 4 "error.log"
-- let b = liftM build a
-- inOrder b

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map messageString filteredMsgs
                     where orderedMsgs = inOrder (build msgs)
                           filteredMsgs = filter messageFilter orderedMsgs

messageFilter :: LogMessage -> Bool
messageFilter (LogMessage (Error level) _ _)
              | level >= 50 = True
              | otherwise = False
messageFilter _ = False

messageString :: LogMessage -> String
messageString (LogMessage _ _ text) = text
messageString _ = ""

-- Example full test
-- testWhatWentWrong parse whatWentWrong "error.log"
-- testWhatWentWrong parse whatWentWrong "sample.log"
