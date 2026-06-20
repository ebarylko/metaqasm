{
{-# LANGUAGE GHC2024 #-}
module Grammar(parseTokens, parseText) where
import Lexer
import Syntax(Expression(..),
              WithContext(..),
              Identifier,
              Idx,
              Id,
              RegisterType(..),
              NatNum,
              NonNeg(..),
              GateApp(..),
              GateArg(..),
              Command(..),
              TermType(..))
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
qreg    {Qreg}
creg    {Creg}
in      {In}
','     {Comma}
':'     {Colon}
gate    {GateDec}
'('     { LParen _}
')'     { RParen _ }
annotation     {TypeAnnotation typ lineNum}
id      { Id name lineNum}
nat     { Nat num lineNum}
measure  {QubitMeasurement}
"->"      {RightArrow}

%%

term :: {Term}
term : command {Vary.from $1}  | arg { Vary.from $1 }

command : qreg id '[' nat ']' in '{' command '}' {DeclRegCollIn Quantum (extractName $2) (toNat $4) $8}
| creg id '[' nat ']' in '{' command '}' {DeclRegCollIn Classical (extractName $2) (toNat $4) $8}
| gateApp {Gate $1}
| gate id '(' gateArgs ')' '{' gateApp '}' in '{' command '}' {DeclGateIn (extractName $2) $4 $7 $11}
| measure arg "->" arg {MeasureQubit $2 $4}

gateArg : id ':' annotation {GateArg (extractName $1) (toTermType $3)}
gateArgs : gateArg {[$1]}
| gateArg ',' gateArgs {$1 : $3}

gateApp : id '(' args ')' {App (toVar $1) $3}

args : arg {[$1]} | arg ',' args {$1 : $3}

arg : id             {(Var . toVar) $1 }
| id '[' nat ']' { RegisterAccess (toVar $1) (toIdx $3) }



{

-- Converts a token representing a variable name to its
-- corresponding term in the grammar
toVar :: Token -> Id
toVar (Id varName lineNum) = WithContext varName lineNum

-- Takes a token representing a type annotation and converts it
-- to the corresponding MetaQASM type
toTermType :: Token -> TermType
toTermType (TypeAnnotation "Qbit" _) = Qbit

toIdx :: Token -> Idx
toIdx (Nat num lineNum) = WithContext (NonNeg num) lineNum

-- Takes a token representing the name of a register collection
-- and extracts the name
extractName :: Token -> Identifier
extractName (Id name _) = name

toNat :: Token -> NatNum
toNat (Nat num ctx) =  WithContext (NonNeg num) ctx

type ParseResult  = Either String

parseError :: [Token] -> ParseResult a
parseError toks = Left $ "The following cannot be parsed: " ++ show toks

parseText :: String -> ParseResult Term

parseText = alexScanTokens >>> parseTokens
}
