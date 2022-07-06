module Main where

import Tracker.App
import RIO
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  app  <- mkApp args
  runApp app