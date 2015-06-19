module Parsers where

import Data.Char8 (isNumber)

-- Auxiliary function to read times
-- remind that ReadS a = String -> [(a,String)]

-- An hour is a pair of chiper followed by a colon (:)
getHours :: ReadS Integer
getHours (x0:x1:':':rest) =
  if (isNumber x0) && (isNumber x1)
  then [((h0*10+h1),rest)]
  else []
  where h0 = read (x0:[])
        h1 = read (x1:[])
-- A minute is written similarly to an hour
getMinutes = getHours

-- seconds are pair of chiphers followed by a comma (,)
getSeconds :: ReadS Integer
getSeconds (x0:x1:rest) =
  if (isNumber x0) && (isNumber x1) 
  then   
    case rest of 
      ',':rest' -> [(s0*10+s1,rest')] -- if the pair of numbers if followed by a comma pair is success
      _ -> []  -- otherwise is a fail
  else
    [] 
  where s0 = read (x0:[])
        s1 = read (x1:[])
        
-- Milliseconds is expressed by 4 ciphers
getMilliSeconds :: ReadS Integer
getMilliSeconds (m0:m1:m2:m3:rest) =
  if all isNumber [m0,m1,m2,m3]
  then [(c3+c2*10+c1*100+c0*1000,rest)]
  else []
  where [c3,c2,c1,c0] = map read [m3:[],m2:[],m1:[],m0:[]]

getTime :: ReadS Integer
getTime string =
  getHours string >>=
  \(h,rest) -> getMinutes rest >>=
               \(m,rest) -> getSeconds rest >>=
                            \(s,rest) -> getMilliSeconds rest >>=
                                         \(c,rest) -> [(c+10^4*s+10^4*60*m+10^4*3600*h,rest)]
                                                      
-- Try to see if the string start with a newline
-- if that's the case it drops the newline, otherwise fails
getNewline :: ReadS ()
getNewline ('\r':'\n':rest) = [((),rest)]
getNewline _ = []

-- Match the separator " --> " for time specification
-- if it is present it simply drops it, otherwise fails
getTimeSep :: ReadS ()
getTimeSep string =
  let (check,rest) = splitAt 5 string
  in
   if check == " --> "
   then [((),rest)]
   else []

-- parse every string
getSubsLines :: ReadS String
getSubsLines string = [(string,"")] 
  
