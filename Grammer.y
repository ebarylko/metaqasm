{
  module Grammer(parseTokens) where

import Lexer
}

%name parseTokens
%tokentype { Token }
%error { parseError }
%lexer { lexText } { EOF }
%monad { Either String } { (>>=) } { return }

%token
'['     { TLBracket }
']'     { TRBracket }
'('     { LParen }
')'     { RParen }
str     { TString $$ }
id      { TID   $$ }
nat     { TNat  $$ }

%%

uop : id '(' arg ')' { GateApp (toVar $1) $3 }

arg : id             { toVar $1 }
    | id '[' nat ']' { toRegAccess $1 $3 }

{


-- This data type represents a metaQASM abstract syntax tree with
-- context annotations in each node
  data Ast context =
    GateApp (Ast context) (Ast context)
    | Var String context
    | RegisterAccess (Ast context) (Ast context) 
    | Index Int context

-- This type represents a token paired with information
-- on which line the token was found
type TokenWithLineInfo = (AlexState, Token)

newType LineNumber = LineNumber Int

-- Extracts the line number from the available
-- line information
getLineNumber :: AlexPosn -> LineNumber
getLineNumber (_, lineNumber, _) = LineNumber lineNumber

-- Coverts a token representing a variable name to its
-- corresponding AST
toVar :: TokenWithLineInfo -> Ast LineNumber

toVar (lineInfo, varName) = Var varName $ getLineNumber lineInfo


toIdx :: TokenWithLineInfo -> Ast LineNumber

toIdx (lineInfo, idx) = Index idx (        getLineNumber lineInfo)


-- Takes two tokens representing the name of a register collection
-- and an index and returns an ast corresponding to a register access
-- of the same collection with the given index
toRegAccess :: TokenWithLineInfo -> TokenWithLineInfo -> Ast LineInfo

toRegAccess regCollToken idxToken = RegisterAccess (toVar regCollToken) (toIdx idxToken)


parseError :: [Token] -> a
parseError xs = error $ "Parse error: " ++ concatMap show xs

}
