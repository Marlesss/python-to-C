module Python.Lexer
  ( Token(..)
  , lexer
  ) where

import Data.Char
import Python.Grammar

data Token
  = TokenInt Int
  | TokenStr String
  | TokenBool Bool
  | TokenVar String
  | TokenType PrimitiveType
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenEq
  | TokenLT
  | TokenLE
  | TokenGT
  | TokenGE
  | TokenEqEq
  | TokenNQ
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenOB
  | TokenCB
  | TokenEOL
  | TokenIf
  | TokenElse
  | TokenOCurly
  | TokenCCurly
  | TokenComma
  | TokenSignat
  | TokenFor
  | TokenIn
  | TokenRIncl
  | TokenRExcl
  deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer ('=':'=':cs) = TokenEqEq   : lexer cs
lexer ('=':cs)     = TokenEq     : lexer cs
lexer ('+':cs)     = TokenPlus   : lexer cs
lexer ('-':cs)     = TokenMinus  : lexer cs
lexer ('*':cs)     = TokenTimes  : lexer cs
lexer ('/':cs)     = TokenDiv    : lexer cs
lexer ('<':'=':cs) = TokenLE     : lexer cs
lexer ('<':cs)     = TokenLT     : lexer cs
lexer ('>':'=':cs) = TokenGE     : lexer cs
lexer ('>':cs)     = TokenGT     : lexer cs
lexer ('!':'=':cs) = TokenNQ     : lexer cs
lexer ('(':cs)     = TokenOB     : lexer cs
lexer (')':cs)     = TokenCB     : lexer cs
lexer ('\n':cs)    = TokenEOL    : lexer cs
lexer ('{':cs)     = TokenOCurly : lexer cs
lexer ('}':cs)     = TokenCCurly : lexer cs
lexer (',':cs)     = TokenComma : lexer cs
lexer (':':':':cs) = TokenSignat : lexer cs
lexer ('.':'.':'.':cs) = TokenRIncl : lexer cs
lexer ('.':'.':'<':cs) = TokenRExcl : lexer cs
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
      | c == '"'  = lexStr cs
      | otherwise = error $ "Unexpected char " ++ [c]

lexNum :: String -> [Token]
lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexStr :: String -> [Token]
lexStr cs = let (str, restWithQuote) = span (\c -> c /= '"' && c /= '\n') cs in
  case restWithQuote of
    '"':rest -> TokenStr str : lexer rest
    _        -> error $ "Expected end of string while parsing string literal"


lexVar :: String -> [Token]
lexVar cs = let (word, rest) = span isAlphaNum cs in
  wordToToken word : lexer rest where
    wordToToken s
      | (not . null) s &&
        (isUpper . head) s = lexPrimitiveType s
      | otherwise          = case s of
           "if"   -> TokenIf
           "else" -> TokenElse
           "for"  -> TokenFor
           "in"   -> TokenIn
           "and"  -> TokenAnd
           "or"   -> TokenOr
           "not"  -> TokenNot
           "true"  -> TokenBool True
           "false"  -> TokenBool False
           var    -> TokenVar var

lexPrimitiveType :: String -> Token
lexPrimitiveType "Int" = TokenType Int
lexPrimitiveType "String" = TokenType String
lexPrimitiveType "Boolean" = TokenType Bool
lexPrimitiveType s = error $ "Unknown primitive type " ++ s