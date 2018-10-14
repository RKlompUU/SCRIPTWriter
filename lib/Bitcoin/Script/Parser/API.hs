module Bitcoin.Script.Parser.API (
  Bitcoin.Script.Parser.SyntaxExtension.languageDescription,
  compileCScript
) where

import Bitcoin.Script.Parser.SyntaxExtension
import Bitcoin.Script.Parser.AST
import Bitcoin.Script.Parser.Parser

-- |'compileCScript' translates a script, written in the custom language, to
-- Bitcoin script bytecode. Returns 'Right' 'String' upon successful translation,
-- and 'Left' 'String' otherwise.
compileCScript :: String -> Either String String
compileCScript scrpt =
  unsugar scrpt
