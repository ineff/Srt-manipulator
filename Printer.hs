module Printer where

-- Auxiliary functions for producing a well typed output

-- showT is an auxiliary function to turn an integer, representing
-- an hour or a minute, into a string of two of digits
showT :: Integer -> String
showT = reverse . (take 2). reverse .('0':) . show

-- showM is an auxiliary function to turn an integer, representing
-- milliseconds, into a string of three digits
showM :: Integer -> String
showM number =
  let num = show number
  in
   ['0'| x <- [(length num)..2]]++num
  

-- showTime take an integer, representing a number of milliseconds,
-- and return a string representing the corrisponding elapsed time
-- in format hh:mm:ss,mmm
showTime :: Integer -> String
showTime time =
  let millisec = time `mod` 1000
      seconds = time `div` 1000
      minutes = seconds `div` 60
      hours = minutes `div` 60
  in
   (showT hours)++":"++(showT $ minutes - hours*60)++":"++(showT $ seconds - minutes*60)++","++(showM millisec)
   
