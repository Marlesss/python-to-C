{
module Python.Parser where

import Python.Grammar
import Python.Lexer
}

%name parse Program
%tokentype { Token }
%error { parseError }

%token
  int  { TokenInt $$  }
  str  { TokenStr $$  }
  var  { TokenVar $$  }
  type { TokenType $$ }
  '='  { TokenEq      }
  '+'  { TokenPlus    }
  '-'  { TokenMinus   }
  '*'  { TokenTimes   }
  '/'  { TokenDiv     }
  '('  { TokenOB      }
  ')'  { TokenCB      }
  '\n' { TokenEOL     }
  if   { TokenIf      }
  else { TokenElse    }
  '{'  { TokenOCurly  }
  '}'  { TokenCCurly  }
  ','  { TokenComma   }
  '::' { TokenSignat  }


%left '+' '-'
%left '*' '/'

%left NEG
%nonassoc '('
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
  | Expr                                         { Expr $1 }

Expr
  : Expr '+' Expr      { Plus $1 $3 }
  | Expr '-' Expr      { Minus $1 $3 }
  | Expr '*' Expr      { Times $1 $3 }
  | Expr '/' Expr      { Div $1 $3 }
  | Expr '(' Args ')'  { Call $1 (reverse $3) }
  | '(' Expr ')'       { Brack $2 }
  | int                { IntVal $1 }
  | str                { StrVal $1 }
  | var                { Var $1 }

Args
  : {- empty -}        { [] }
  | ArgsTail Expr         { $2 : $1 }

ArgsTail
  : {- empty -}        { [] }
  | ArgsTail Expr ','     { $2 : $1 }

{
parseError _ = error "Parse error"
}