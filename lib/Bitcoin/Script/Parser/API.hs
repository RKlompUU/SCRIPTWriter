module Bitcoin.Script.Parser.API (
  Bitcoin.Script.Parser.SyntaxExtension.languageDescription,
  Bitcoin.Script.Parser.AST.ScriptAST(..),
  eScriptToBytecode,
  eScriptToScriptOps,
  eScriptToAST
) where

import Bitcoin.Script.Parser.SyntaxExtension
import Bitcoin.Script.Parser.AST
import Bitcoin.Script.Parser.Parser

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Bitcoin.Script

-- |'eScriptToBytecode' translates a script, written in the extended language, to
-- Bitcoin script bytecode (in 'String' format). Returns 'Right' 'String' upon successful translation,
-- and 'Left' 'String' otherwise.
eScriptToBytecode :: String -> Either String String
eScriptToBytecode cScrpt =
  unsugar cScrpt

-- |'eScriptToScriptOps' translates a script, written in the extended language, to
-- a list of SCRIPT instructions. Returns 'Right' ['ScriptOp'] upon successful translation,
-- and 'Left' 'String' otherwise.
eScriptToScriptOps :: String -> Either String [ScriptOp]
eScriptToScriptOps cScrpt =
  scriptOps
  <$> decode
  <$> B.pack
  <$> eScriptToBytecode cScrpt

-- |'eScriptToAST' translates a script, written in the extended language, to
-- an Abstract Syntax Tree (with nested true and false branches of If Then Else).
-- Returns 'Right' 'ScriptAST' upon successful translation, and 'Left' 'String' otherwise.
eScriptToAST :: String -> Either String ScriptAST
eScriptToAST cScrpt =
  runFillLabels
  <$> buildAST
  <$> eScriptToScriptOps cScrpt
