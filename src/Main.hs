module Main where

import Parse

import qualified Data.ByteString.Lazy as B

main :: IO ()
main = do
  contents <- B.readFile "progs.dat"
  case parseProgs contents of
    Left err -> putStrLn err
    Right progs -> print progs
