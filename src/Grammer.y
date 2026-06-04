{
  module Grammar(parseTokens) where
import Lexer
}

%name parseTokens
%tokentype { Token }
%monad { ParseResult } { (>>=) } { return }

%token
'['     { LBracket _}
']'     { RBracket _}
'('     { LParen _}
')'     { RParen _ }
str     { Str s lineNum}
id      { ID name lineNum}
nat     { Nat num lineNum}

%%

uop : id '(' arg ')' { GateApp (toVar $1) $3 }

arg : id             { toVar $1 }
    | id '[' nat ']' { RegisterAccess (toVar $1) (toIdx $3) }

{


-- This data type represents a metaQASM abstract syntax tree with
-- line number annotations in each node
  data Ast  =
  GateApp{gate:: Ast, arg::Ast}
  | Var String LineNum
  | RegisterAccess{collection :: Ast, regIdx :: Ast}
  | Index Int LineNum

-- Coverts a token representing a variable name to its
-- corresponding AST
toVar :: Token -> Ast

toVar (Id varName lineNum) = Var varName lineNum

toIdx :: Token -> Ast
toIdx (Nat num lineNum) = Index num lineNum

type ParseResult  = Either String

parseError :: [Token] -> ParseResult a
parseError (x : _) = Left $ "The following cannot be parsed: " ++ show x 
}
