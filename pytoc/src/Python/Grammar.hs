module Python.Grammar
  ( Program
  , PrimitiveType(..)
  , Statement(..)
  , Expr(..)
  , Range(..)
  , VarName
  ) where

type Program = [Statement]

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
  | If Expr Program
  | IfElse Expr Program Program
  | For VarName Range Program
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
