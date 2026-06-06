{
module Grammar(parseTokens) where
import Lexer
import Syntax(Expression(..),
           WithContext(..),
           Index(..),
           Idx,
           Id)
}

%name parseTokens
%tokentype { Token }
%error { parseError }
%monad { ParseResult } { (>>=) } { return }

%token
'['     { LBracket _}
']'     { RBracket _}
'('     { LParen _}
')'     { RParen _ }
str     { Str s lineNum}
id      { Id name lineNum}
nat     { Nat num lineNum}

%%

arg : id             {(Var . toVar) $1 }
    | id '[' nat ']' { RegisterAccess (toVar $1) (toIdx $3) }

{

-- Converts a token representing a variable name to its
-- corresponding term in the grammar
toVar :: Token -> Id
toVar (Id varName lineNum) = WithContext varName lineNum

toIdx :: Token -> Idx
toIdx (Nat num lineNum) = WithContext (Index num) lineNum

type ParseResult  = Either String

parseError :: [Token] -> ParseResult a
parseError toks = Left $ "The following cannot be parsed: " ++ show toks
}
