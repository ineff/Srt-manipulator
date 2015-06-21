{-# LANGUAGE OverloadedStrings #-}

module Srt (Srt,
            shiftTime,
            enlargeTime,
            changeSub) where

import Parsers 
import Printer

data Srt = Srt {
  sub :: String,
  index :: Integer,
  start :: Integer,
  end :: Integer
  } deriving(Eq)


-- shiftTime srt shift return the sub obtained
-- by delaying the sub of shift milliseconds
-- (the shift can also be negative)
shiftTime :: Integer -> Srt -> Srt
shiftTime shift srt = srt {
  start = (start srt)+shift,
  end = (end srt)+shift
  }

-- enlargeTime str grow return the sub obtained
-- by enlarging the duration of the subtitle
-- (grow can be negative)
enlargeTime :: Integer -> Srt -> Srt
enlargeTime grow srt = srt {
  end = (end srt)+grow
  }

-- changeSub srt sub change the sub text
changeSub :: String -> Srt -> Srt
changeSub text srt = srt {
  sub = text
  }


-- Instance of the Ord typeclass for Srt
-- subs are ordered by their index
instance Ord Srt where 
  compare fst snd = compare (index fst) (index snd)

-- Instance of the Read typeclass for Srt
instance Read Srt where
  readsPrec _ string = do
    (index,rest) <- (readsPrec 0 :: ReadS Integer) string
    (_,rest) <- getNewline rest
    (start,rest) <- getTime rest
    (_,rest) <- getTimeSep rest
    (end,rest) <- getTime rest
    (_,rest) <- getNewline rest
    (subString,rest) <- getSubsLines rest
    return (Srt{ index=index,
                 start=start,
                 end=end,
                 sub=subString
               }, rest)
      
  readList string = 
    let 
      tailrec [] string =
        case (readsPrec 0 :: ReadS Srt) string of -- if we can read any srt we fail
          [] -> [] 
          (srt,rest):_ -> tailrec (srt:[]) rest   -- otherwise we continue to look for other subs
      tailrec list string = -- if we have read some srt
        case (readsPrec 0 :: ReadS Srt) string of
          [] -> [(reverse list,string)] -- we have find all the srt we could find
          (srt,rest'):_ -> tailrec (srt:list) rest' -- we try to look for other subs
    in tailrec [] string

-- Instance of the Show class for SRT
instance Show Srt where
  showsPrec _ srt = \s ->
    (show $ index srt)++"\r\n"++
    (showTime $ start srt)++" --> "++(showTime $ end srt)++"\r\n"++
    (sub srt)++"\r\n\r\n"
  showList list string = (concat (map show list)) ++string


