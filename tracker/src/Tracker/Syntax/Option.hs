module Tracker.Syntax.Option
  ( unNone
  ) where

import RIO

unNone :: [Maybe a] -> [a]
unNone elems =
  foldr (\x acc -> 
    case x of
      Just x1 -> x1:acc
      _       -> acc
    ) [] elems