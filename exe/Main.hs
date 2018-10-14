module Main where

import System.IO
import System.Exit
import System.Environment

import Data.List
import Data.Maybe

import Bitcoin.Script.Parser.API

printHelp :: IO ()
printHelp = do
  putStrLn $ "Send the script, written in the extended language, through the std_in, and don't pass any arguments.\n" ++ languageDescription
  exitFailure

readStdin :: IO String
readStdin = do
  done <- isEOF
  if done
    then return ""
    else do
      line <- getLine
      lines <- readStdin
      return $ line ++ ('\n' : lines)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      res <- cScriptToBytecode <$> readStdin
      case res of
        Left err -> putStrLn err
        Right scrpt -> putStrLn scrpt
      return ()
    else printHelp
