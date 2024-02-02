module Clang.Grammar
  ( Program(..)
  , ImportBlock
  , Library(..)
  , Statement(..)
  , PrimitiveType(..)
  , Expr(..)
  , VarName
  , Block
  ) where

import Prettyprinter

data Program = Program ImportBlock MainBlock deriving Show

type Block = [Statement]
newtype Library = Library String deriving Show
type ImportBlock = [Library]
type MainBlock = Block

data PrimitiveType
  = Int
  | String
  deriving Show
type VarName = String

data Statement
  = Define VarName PrimitiveType
  | DefineSet VarName PrimitiveType Expr
  | Set VarName Expr
  | Expr Expr
  | If Expr Block
  | IfElse Expr Block Block
  deriving Show

data Expr
  = Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Div Expr Expr
  | Call Expr [Expr]
  | Brack Expr
  | IntVal Int
  | StrVal String
  | Var VarName
  deriving (Show, Eq, Ord)

instance Semigroup Program where
  (Program impB1 mainB1) <> (Program impB2 mainB2) = Program (impB1 <> impB2) (mainB1 <> mainB2)

instance Monoid Program where
  mempty = Program [] []

tabWidth :: Int
tabWidth = 4

instance Pretty Expr where
  pretty (Plus e1 e2)  = prettyBinary e1 "+" e2
  pretty (Minus e1 e2) = prettyBinary e1 "-" e2
  pretty (Times e1 e2) = prettyBinary e1 "*" e2
  pretty (Div e1 e2)   = prettyBinary e1 "/" e2
  pretty (Call e args) = pretty e <> parens (mconcat (punctuate (comma <> space) $ map pretty args))
  pretty (Brack e)     = parens $ pretty e
  pretty (IntVal x)    = pretty x
  pretty (StrVal s)    = dquotes $ pretty s
  pretty (Var name)    = pretty name

prettyBinary :: Expr -> String -> Expr -> Doc a
prettyBinary e1 d e2 = parens $ pretty e1 <+> pretty d <+> pretty e2

instance Pretty Statement where
  pretty (Define name varType)        = pretty varType <+> pretty name <> semi
  pretty (DefineSet name varType val) = pretty varType <+> pretty name <+> equals <+> pretty val <> semi
  pretty (Set name val)               = pretty name <+> equals <+> pretty val <> semi
  pretty (Expr e)                     = pretty e <> semi
  pretty (If cond block)              = pretty "if" <+> parens (pretty cond) <+> prettyBlock block
  pretty (IfElse cond b1 b2)          = pretty "if" <+> parens (pretty cond) <+> prettyBlock b1
                                                    <+> pretty "else" <+> prettyBlock b2

instance Pretty PrimitiveType where
  pretty Int = pretty "int"
  pretty String = pretty "char*"

prettyBlock :: Block -> Doc a
prettyBlock stmts = lbrace <> line <> (indent tabWidth (vsep (map pretty stmts))) <> line <> rbrace

instance Pretty Program where
  pretty (Program impB mainB) = concatWith (surround hardline) (map pretty impB) <> hardline <> hardline <>
                                pretty "int main()" <+> prettyBlock mainB <> hardline

instance Pretty Library where
  pretty (Library lib) = pretty "#include <" <> pretty lib <> pretty ">"