module Clang.Grammar
  ( Program(..)
  , ImportBlock
  , Library(..)
  , Statement(..)
  , PrimitiveType(..)
  , Expr(..)
  , Range(..)
  , VarName
  , Block
  ) where

import Prettyprinter
import Prelude hiding (LT, GT, EQ)

data Program = Program ImportBlock MainBlock deriving Show

type Block = [Statement]
newtype Library = Library String deriving Show
type ImportBlock = [Library]
type MainBlock = Block

data PrimitiveType
  = Int
  | String
  | Bool
  deriving Show
type VarName = String

data Statement
  = Define VarName PrimitiveType
  | DefineSet VarName PrimitiveType Expr
  | Set VarName Expr
  | Expr Expr
  | If Expr Block
  | IfElse Expr Block Block
  | For VarName Range Block
  deriving Show

data Expr
  = Plus Expr Expr
  | Minus Expr Expr
  | Times Expr Expr
  | Div Expr Expr
  | LT Expr Expr
  | LE Expr Expr
  | GT Expr Expr
  | GE Expr Expr
  | EQ Expr Expr
  | NQ Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Call Expr [Expr]
  | Brack Expr
  | IntVal Int
  | StrVal String
  | BoolVal Bool
  | Var VarName
  deriving (Show, Eq, Ord)

data Range
  = Inclusive {from :: Expr, to :: Expr}
  | Exclusive {from :: Expr, to :: Expr}
  deriving Show


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
  pretty (LT e1 e2)    = prettyBinary e1 "<" e2
  pretty (LE e1 e2)    = prettyBinary e1 "<=" e2
  pretty (GT e1 e2)    = prettyBinary e1 ">" e2
  pretty (GE e1 e2)    = prettyBinary e1 ">=" e2
  pretty (EQ e1 e2)    = prettyBinary e1 "==" e2
  pretty (NQ e1 e2)    = prettyBinary e1 "!=" e2
  pretty (And e1 e2)   = prettyBinary e1 "&&" e2
  pretty (Or e1 e2)    = prettyBinary e1 "||" e2
  pretty (Not e)       = pretty "!" <> parens (pretty e)
  pretty (Call e args) = pretty e <> parens (mconcat (punctuate (comma <> space) $ map pretty args))
  pretty (Brack e)     = parens $ pretty e
  pretty (IntVal x)    = pretty x
  pretty (StrVal s)    = dquotes $ pretty s
  pretty (BoolVal v)    = pretty (if v then "true" else "false")
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
  pretty (For name range block)       = let var = Var name in
                                        pretty "for" <+> parens (
                                                          pretty (DefineSet name Int (from range))
                                                          <+> pretty (comp var (to range)) <> semi
                                                          <+> pretty "++" <> pretty name
                                                   ) <+> prettyBlock block
                                        where
                                          comp = case range of
                                            (Inclusive _ _) -> LE
                                            (Exclusive _ _) -> LT

instance Pretty PrimitiveType where
  pretty Int = pretty "int"
  pretty String = pretty "char*"
  pretty Bool = pretty "bool"

prettyBlock :: Block -> Doc a
prettyBlock stmts = lbrace <> line <> (indent tabWidth (vsep (map pretty stmts))) <> line <> rbrace

instance Pretty Program where
  pretty (Program impB mainB) = concatWith (surround hardline) (map pretty impB) <> hardline <> hardline <>
                                pretty "int main()" <+> prettyBlock mainB <> hardline

instance Pretty Library where
  pretty (Library lib) = pretty "#include <" <> pretty lib <> pretty ">"