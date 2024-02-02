{
module Python.Parser where

import Prelude hiding (LT, GT, EQ)
import Python.Grammar
import Python.Lexer
}

%name parse Program
%tokentype { Token }
%error { parseError }

%token
  int   { TokenInt $$  }
  str   { TokenStr $$  }
  bool  { TokenBool $$ }
  var   { TokenVar $$  }
  type  { TokenType $$ }
  '='   { TokenEq      }
  '+'   { TokenPlus    }
  '-'   { TokenMinus   }
  '*'   { TokenTimes   }
  '/'   { TokenDiv     }
  '<'   { TokenLT      }
  '<='  { TokenLE      }
  '>'   { TokenGT      }
  '>='  { TokenGE      }
  '=='  { TokenEqEq    }
  '!='  { TokenNQ      }
  'and' { TokenAnd     }
  'or'  { TokenOr      }
  'not' { TokenNot     }
  '('   { TokenOB      }
  ')'   { TokenCB      }
  '\n'  { TokenEOL     }
  if    { TokenIf      }
  else  { TokenElse    }
  '{'   { TokenOCurly  }
  '}'   { TokenCCurly  }
  ','   { TokenComma   }
  '::'  { TokenSignat  }
  for   { TokenFor     }
  in    { TokenIn      }
  '...' { TokenRIncl   }
  '..<' { TokenRExcl   }

%nonassoc '<' '<=' '>' '>=' '==' '!='

%left '+' '-'
%left '*' '/'
%right 'and' 'or'

%left NEG
%nonassoc '('
%nonassoc 'not'
%%

Program
 : Statements { reverse $1 }

Statements
 : {- empty -}            { [] }
 | Statement              { [$1] }
 | Statements '\n'           { $1 }
 | Statements '\n' Statement { $3 : $1 }

Statement
  : var '::' type                                { Define $1 $3 }
  | var '::' type '=' Expr                       { DefineSet $1 $3 $5 }
  | var '=' Expr                                 { Set $1 $3 }
  | if Expr '{' Program '}'                      { If $2 $4}
  | if Expr '{' Program '}' else '{' Program '}' { IfElse $2 $4 $8 }
  | for var in Range '{' Program '}'             { For $2 $4 $6}
  | Expr                                         { Expr $1 }

Expr
  : Expr '+' Expr      { Plus $1 $3 }
  | Expr '-' Expr      { Minus $1 $3 }
  | Expr '*' Expr      { Times $1 $3 }
  | Expr '/' Expr      { Div $1 $3 }
  | Expr '<' Expr      { LT $1 $3 }
  | Expr '<=' Expr     { LE $1 $3 }
  | Expr '>' Expr      { GT $1 $3 }
  | Expr '>=' Expr     { GE $1 $3 }
  | Expr '==' Expr     { EQ $1 $3 }
  | Expr '!=' Expr     { NQ $1 $3 }
  | Expr 'and' Expr    { And $1 $3 }
  | Expr 'or' Expr     { Or $1 $3 }
  | 'not' Expr         { Not $2 }
  | Expr '(' Args ')'  { Call $1 (reverse $3) }
  | '(' Expr ')'       { Brack $2 }
  | int                { IntVal $1 }
  | bool               { BoolVal $1 }
  | str                { StrVal $1 }
  | var                { Var $1 }

Args
  : {- empty -}        { [] }
  | ArgsTail Expr         { $2 : $1 }

ArgsTail
  : {- empty -}        { [] }
  | ArgsTail Expr ','     { $2 : $1 }

Range
  : Expr '...' Expr { Inclusive $1 $3 }
  | Expr '..<' Expr { Exclusive $1 $3 }

{
parseError _ = error "Parse error"
}