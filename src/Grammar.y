{
{-# LANGUAGE GHC2024 #-}
module Grammar(parseTokens, parseText) where
import Lexer
import Syntax(Expression(..),
              WithContext(..),
              Identifier,
              Idx,
              Id,
              GateInfo(..),
              RegisterType(..),
              RegCollInfo(..),
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
';'     {Semicolon}
reset {Reset}
gate    {GateDec}
'('     { LParen _}
')'     { RParen _ }
simpleAnnotation     {SimpleTypeAnnotation typ lineNum}
id      { Id name lineNum}
nat     { Nat num lineNum}
measure  {Measurement}
"->"      {RightArrow}

%%

term :: {Term}
term : command {Vary.from $1}  | arg { Vary.from $1 }

command : qreg id '[' nat ']' in '{' command '}' {ScopedRegCollDecl (RegCollInfo Quantum (extractName $2) (toNat $4)) $8}
| creg id '[' nat ']' in '{' command '}' {ScopedRegCollDecl (RegCollInfo Classical (extractName $2) (toNat $4)) $8}
| qreg id '[' nat ']' {RegCollDecl (RegCollInfo Quantum (extractName $2) (toNat $4))}
| creg id '[' nat ']' {RegCollDecl (RegCollInfo Classical (extractName $2) (toNat $4))}
| gateApp {Gate $1}
| gate id '(' gateArgs ')' '{' gateApp '}' in '{' command '}' {ScopedGateDecl (GateInfo (extractName $2) $4 $7) $11}
| measure arg "->" arg {QubitMeasurement $2 $4}
| command ';' command {Sequence $1 $3}
| reset arg {QubitReset $2}
| gate id '(' gateArgs ')' '{' gateApp '}' {GateDecl (GateInfo (extractName $2) $4 $7)}

compoundType : simpleAnnotation '[' nat ']' {RegisterGroup ((toRegCollType  . toTermType) $1) $ toNat $3}
type : simpleAnnotation {toTermType $1} | compoundType {$1}

gateArg : id ':' type {GateArg (extractName $1) $3}

gateArgs : gateArg {[$1]}
| gateArg ',' gateArgs {$1 : $3}

gateApp : id '(' args ')' {GateApp (toVar $1) $3}

args : arg {[$1]} | arg ',' args {$1 : $3}

arg : id             {(Var . toVar) $1 }
| id '[' nat ']' { RegisterAccess (toVar $1) (toIdx $3) }


{

toRegCollType :: TermType -> RegisterType
toRegCollType Qbit = Quantum
toRegCollType Bit = Classical

-- Converts a token representing a variable name to its
-- corresponding term in the grammar
toVar :: Token -> Id
toVar (Id varName lineNum) = WithContext varName lineNum

-- Takes a token representing a type annotation and converts it
-- to the corresponding MetaQASM type
toTermType :: Token -> TermType
toTermType (SimpleTypeAnnotation "Qbit" _) = Qbit
toTermType (SimpleTypeAnnotation "Bit" _) = Bit

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
