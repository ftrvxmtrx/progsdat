module Main where

import Parse

import Types
import Util

import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  contents <- L.readFile "progs.dat"
  case parseProgs contents of
    Left err -> putStrLn err
    Right progs -> do
      mapM_ (\f -> case f of
                     Func _ _ _ _ -> print f >> dumpOps progs (funcStart f) >> putStrLn ""
                     _ -> return ()
            ) $ progsFuncs progs
