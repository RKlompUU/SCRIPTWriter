module Bitcoin.Script.Parser.API (
  Bitcoin.Script.Parser.SyntaxExtension.languageDescription,
  module Bitcoin.Script.Parser.AST,
  cScriptToBytecode,
  cScriptToScriptOps,
  cScriptToAST
) where

import Bitcoin.Script.Parser.SyntaxExtension
import Bitcoin.Script.Parser.AST
import Bitcoin.Script.Parser.Parser

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Bitcoin.Script

-- |'customScript2Bytecode' translates a script, written in the custom language, to
-- Bitcoin script bytecode. Returns 'Right' 'String' upon successful translation,
-- and 'Left' 'String' otherwise.
cScriptToBytecode :: String -> Either String String
cScriptToBytecode cScrpt =
  unsugar cScrpt

-- |'cScriptToScriptOps' translates a script, written in the custom language, to
-- a list of SCRIPT instructions. Returns 'Right' 'String' upon successful translation,
-- and 'Left' 'String' otherwise.
cScriptToScriptOps :: String -> Either String [ScriptOp]
cScriptToScriptOps cScrpt =
  scriptOps
  <$> decode
  <$> B.pack
  <$> cScriptToBytecode cScrpt

-- |'cScriptToAST' translates a script, written in the custom language, to
-- an Abstract Syntax Tree (with nested true and false branches of If Then Else).
-- Returns 'Right' 'String' upon successful translation, and 'Left' 'String' otherwise.
cScriptToAST :: String -> Either String ScriptAST
cScriptToAST cScrpt =
  runFillLabels
  <$> buildAST
  <$> cScriptToScriptOps cScrpt
