module Main where

import Parse

import Types
import Util

import Data.ByteString.Char8 (pack)
import qualified Data.ByteString.Lazy as L
import Data.Maybe

main :: IO ()
main = do
  contents <- L.readFile "progs.dat"
  case parseProgs contents of
    Left err -> putStrLn err
    Right progs -> do
      let f = fromJust $ findFunc progs (pack "TraceAttack")
      print f
      dumpOps progs (funcStart f)
