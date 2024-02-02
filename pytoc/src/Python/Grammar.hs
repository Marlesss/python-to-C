module Python.Grammar
  ( Program
  , PrimitiveType(..)
  , Statement(..)
  , Expr(..)
  , VarName
  ) where

type Program = [Statement]

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
  | If Expr Program
  | IfElse Expr Program Program
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

