{
{-# LANGUAGE GHC2024 #-}
module Grammar(parseTokens, parseText) where
import Lexer
import Syntax(Expression(..),
              WithContext(..),
              Identifier,
              Idx,
              Id,
              NatNum,
              NonNeg(..),
              GateApp(..),
              Command(..))
import qualified Vary
import Typecheck(Term)
import Control.Arrow((>>>))
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
','     {Comma}
'('     { LParen _}
')'     { RParen _ }
str     { Str s lineNum}
id      { Id name lineNum}
nat     { Nat num lineNum}

%%

term :: {Term}
term : command {Vary.from $1}  | arg { Vary.from $1 }

command : creg id '[' nat ']' in '{' command '}' {QRegDeclIn (toRegCollName $2) (toNat $4) $8} |
gateApp {Gate $1}

gateApp : id '(' arg ')' {(toGate $1) $3} | id '(' arg ',' arg ')' {ControlledNot $3 $5}

arg : id             {(Var . toVar) $1 }
| id '[' nat ']' { RegisterAccess (toVar $1) (toIdx $3) }


{
-- Converts a token representing a variable name to its
-- corresponding term in the grammar
toVar :: Token -> Id
toVar (Id varName lineNum) = WithContext varName lineNum

toIdx :: Token -> Idx
toIdx (Nat num lineNum) = WithContext (NonNeg num) lineNum

type SingleQubitUnitary = Expression -> GateApp
toGate :: Token -> SingleQubitUnitary
-- Takes a token representing a gate and returns the
-- gate corresponding to it
toGate (Id "h" _) = H
toGate (Id "t" _) = T
toGate (Id "tdg" _) = Tdg

-- Takes a token representing the name of a register collection
-- and extracts the name
toRegCollName :: Token -> Identifier
toRegCollName (Id name _) = name

toNat :: Token -> NatNum
toNat (Nat num ctx) =  WithContext (NonNeg num) ctx

type ParseResult  = Either String

parseError :: [Token] -> ParseResult a
parseError toks = Left $ "The following cannot be parsed: " ++ show toks

parseText :: String -> ParseResult Term

parseText = alexScanTokens >>> parseTokens
}
