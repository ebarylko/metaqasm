{
{-# LANGUAGE GHC2024 #-}
module Grammar(parseTokens, parseText) where
import Lexer
import Syntax(Expression(..),
              WithContext(..),
              Identifier,
              Index(..),
              Idx,
              Id,
              GateInfo(..),
              RegisterType(..),
              RegCollInfo(..),
              GateApp(..),
              GateArg(..),
              Command(..),
              TermType(..))
import qualified Vary
import Typecheck(Term)
import Control.Arrow((>>>))
import Data.Function((&), on)
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
if      {If}
Circuit {Circ}
','     {Comma}
':'     {Colon}
';'     {Semicolon}
'+'     {Plus lineNum}
'-'     {Minus lineNum}
"=="     {Eq}
reset {Reset}
gate    {GateDec}
'('     { LParen _}
')'     { RParen _ }
simpleAnnotation     {SimpleTypeAnnotation typ lineNum}
id      { Id name lineNum}
nat     { Nat num lineNum}
measure  {Measurement}
"->"      {RightArrow}

%left '+' '-'

%%


term :: {Term}
term : command {Vary.from $1}  | arg { Vary.from $1 }


command : qreg id '[' idx ']' in '{' command '}' {ScopedRegCollDecl (RegCollInfo Quantum (extractName $2) $4) $8}
| creg id '[' idx ']' in '{' command '}' {ScopedRegCollDecl (RegCollInfo Classical (extractName $2) $4) $8}
| qreg id '[' idx ']' {RegCollDecl (RegCollInfo Quantum (extractName $2) $4)}
| creg id '[' idx ']' {RegCollDecl (RegCollInfo Classical (extractName $2) $4)}
| gateApp {Gate $1}
| gate id '(' gateArgs ')' '{' gateApp '}' in '{' command '}' {ScopedGateDecl (GateInfo (extractName $2) $4 $7) $11}
| measure arg "->" arg {QubitMeasurement $2 $4}
| command ';' command {Sequence $1 $3}
| reset arg {QubitReset $2}
| gate id '(' gateArgs ')' '{' gateApp '}' {GateDecl (GateInfo (extractName $2) $4 $7)}
| if '(' arg  "==" idx ')' '{' gateApp '}' {ConditionalGateExec $3 $8}

compoundType :
simpleAnnotation '[' idx ']' {RegisterGroup ((toRegCollType  . toTermType) $1) $ $3}
| Circuit '(' types ')' {Circuit $3}

types : type {[$1]}
| type ',' types {$1 : $3}
type : simpleAnnotation {toTermType $1} | compoundType {$1}

gateArg : id ':' type {GateArg (extractName $1) $3}

gateArgs : gateArg {[$1]}
| gateArg ',' gateArgs {$1 : $3}

gateApp : id '(' args ')' {GateApp (toVar $1) $3}
| gateApp ';' gateApp {GateSequence $1 $3}

args : arg {[$1]} | arg ',' args {$1 : $3}

idx : nat {toIdx $1}
| idx '+' idx {(toIdxSum (extractLineNum $2) `on` extractVal) $1  $3}
| idx '-' idx {(toIdxDiff (extractLineNum $2) `on` extractVal) $1  $3}

arg : id             {(Var . toVar) $1 }
| id '[' idx ']' { RegisterAccess (toVar $1) $3 }


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

extractVal :: WithContext a b -> a
extractVal (WithContext x _) = x

toIdx :: Token -> Idx
toIdx x@(Nat _ lineNum) = toIndex x & flip WithContext lineNum
  where
    toIndex :: Token -> Index
    toIndex (Nat num _) = Const  num

extractLineNum :: Token -> LineNumber
extractLineNum (Plus line) = line
extractLineNum (Minus line) = line

-- Takes the context for when a summation occurred,
-- the tokens representing the operands, and returns
-- a term that represents the summation of both operands
toIdxSum :: LineNumber -> Index -> Index -> Idx
toIdxSum line num1 = flip WithContext line . Sum num1

-- Takes the context for when a difference occurred,
-- the tokens representing the operands, and returns
-- a term that represents the difference of both operands
toIdxDiff :: LineNumber -> Index -> Index -> Idx
toIdxDiff line num1 = flip WithContext line . Diff num1

-- Takes a token representing the name of a register collection
-- and extracts the name
extractName :: Token -> Identifier
extractName (Id name _) = name

type ParseResult  = Either String

parseError :: [Token] -> ParseResult a
parseError toks = Left $ "The following cannot be parsed: " ++ show toks

parseText :: String -> ParseResult Term

parseText = alexScanTokens >>> parseTokens
}
