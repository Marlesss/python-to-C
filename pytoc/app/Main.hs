module Main (main) where

import Python.Parser
import Python.Lexer
import Translate
import Prettyprinter
import Prettyprinter.Render.Text
import System.IO

main :: IO ()
main = do
  pythonCode <- readFile "code.py"
  let parsed = parse $ lexer pythonCode
  let translated = pyToC parsed
  withFile "out.c" WriteMode $ (flip hPutDoc) $ pretty translated
