module Tracker.Syntax.Option
  ( unNone
  ) where

import RIO

unNone :: [a] -> [Maybe a] -> [a]
unNone acc (x:xs) =
  case x of 
    Just x1 -> unNone (x1:acc) xs
    _       -> unNone acc xs 
unNone acc [] = acc