{-# LANGUAGE OverloadedStrings #-}

module Srt where

import qualified Data.ByteString.Char8 as BS -- to use ByteStrings and avoid problem with char-encoding
import Parsers 

data Srt = Srt {
  sub :: BS.ByteString,
  index :: Integer,
  start :: Integer,
  end :: Integer
  } deriving(Eq)

showT :: Integer -> String
showT = reverse . (take 2). reverse .('0':) . show

showM :: Integer -> String
showM number =
  let num = show number
  in
   ['0'| x <- [(length num)..3]]++num
  
showTime :: Integer -> String
showTime time =
  let millisec = time `mod` 10000
      seconds = time `div` 10000
      minutes = seconds `div` 60
      hours = minutes `div` 60
  in
   (showT hours)++":"++(showT $ minutes - hours*60)++":"++(showT $ seconds - minutes*60)++","++(showM millisec)
   
instance Read Srt where
  readsPrec _ string = do
    (index,rest) <- (readsPrec 0 :: ReadS Integer) string
    (_,rest) <- getNewline rest
    (start,rest) <- getTime rest
    (_,rest) <- getTimeSep rest
    (end,rest) <- getTime rest
    (_,rest) <- getNewline rest
    (subString,rest) <- getSubsLines rest
    (_,rest) <- getNewline rest
    (_,rest) <- getNewline rest
    return (Srt{ index=index,
                 start=start,
                 end=end,
                 sub=BS.pack subString
               }, rest)
  
instance Show Srt where
  showsPrec _ srt = \s ->
    (show $ index srt)++"\r\n"++
    (showTime $ start srt)++" --> "++(showTime $ end srt)++"\r\n"++
    (BS.unpack $ sub srt)++"\r\n"
    
