{
{-# LANGUAGE GHC2024 #-}
module Grammar(parseTokens) where
import Lexer
import Syntax(Expression(..),
              WithContext(..),
              Index(..),
              Identifier,
              Idx,
              PosNum(..),
              Id,
              GateApp(..),
              Command(..))
import qualified Vary
import Typecheck(Term)
}

%name parseTokens 
%tokentype { Token }
%error { parseError }
%monad { ParseResult } { (>>=) } { return }

%token
'['     { LBracket _}
']'     { RBracket _}
'{'     { LCurlyBracket _}
'}'     { RCurlyBracket _}
creg    {Creg}
in      {In}
'('     { LParen _}
')'     { RParen _ }
str     { Str s lineNum}
id      { Id name lineNum}
nat     { Nat num lineNum}
pos     { Pos num lineNum}

%%

term :: {Term}
term : command {Vary.from $1} | gateApp {Vary.from $1 } | arg { Vary.from $1 }

command : creg id '[' pos ']' in '{' command '}' {QRegDeclIn (toRegCollName $2) (toPos $4) $8} |
gateApp {Gate $1}

gateApp : id '(' arg ')' {H $3}

arg : id             {(Var . toVar) $1 }
| id '[' nat ']' { RegisterAccess (toVar $1) (toIdx $3) }


{
-- Converts a token representing a variable name to its
-- corresponding term in the grammar
toVar :: Token -> Id
toVar (Id varName lineNum) = WithContext varName lineNum

toIdx :: Token -> Idx
toIdx (Nat num lineNum) = WithContext (Index num) lineNum

-- Takes a token representing the name of a register collection
-- and extracts the name
toRegCollName :: Token -> Identifier
toRegCollName (Id name _) = name

toPos :: Token -> PosNum
toPos (Pos num _) = PosNum num

type ParseResult  = Either String

parseError :: [Token] -> ParseResult a
parseError toks = Left $ "The following cannot be parsed: " ++ show toks
}
