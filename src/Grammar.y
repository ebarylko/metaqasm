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
'*'     {Times lineNum}
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
%left '*'

%%


term :: {Term}
term : command {Vary.from $1}  | arg { Vary.from $1 }


command : qreg id '[' idx ']' in '{' command '}' {ScopedRegCollDecl (genQuantumRegCollInfo  $2 $4) $8}
| creg id '[' idx ']' in '{' command '}' {ScopedRegCollDecl (genClassicalRegCollInfo  $2 $4) $8}
| qreg id '[' idx ']' {RegCollDecl (genQuantumRegCollInfo  $2 $4)}
| creg id '[' idx ']' {RegCollDecl (genClassicalRegCollInfo  $2 $4)}
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
| idx '+' idx {toIdxSum $2 $1 $3}
| idx '-' idx {toIdxDiff $2 $1 $3}
| idx '*' idx {toIdxProd $2 $1 $3}

arg : id             {(Var . toVar) $1 }
| id '[' idx ']' { RegisterAccess (toVar $1) $3 }


{

-- | idx '+' idx {(toIdxSum (extractLineNum $2) `on` extractVal) $1  $3}

genRegCollInfo :: RegisterType -> Token -> Idx -> RegCollInfo
genRegCollInfo collKind regCollName numOfRegs = RegCollInfo collKind (extractName regCollName) numOfRegs
genQuantumRegCollInfo ::  Token -> Idx -> RegCollInfo
genQuantumRegCollInfo = genRegCollInfo Quantum

genClassicalRegCollInfo :: Token -> Idx -> RegCollInfo
genClassicalRegCollInfo = genRegCollInfo Classical


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
extractLineNum (Times line) = line


-- Takes a binary operation on indices,
-- a token representing the binary operation,  the
-- arguments to the operation, and returns an index
-- representing the result of applying the operation on
-- the given indices
toBinIdxOp :: (Index -> Index -> Index) -> Token  -> Idx -> Idx -> Idx

toBinIdxOp  op opTok fstArg sndArg = (toBinIdxOp' op (extractLineNum opTok) `on` extractVal) fstArg sndArg
  where
    toBinIdxOp' :: (Index -> Index -> Index) -> LineNumber -> Index -> Index -> Idx
    toBinIdxOp' op line fstIdx = flip WithContext line . op fstIdx

toIdxSum :: Token -> Idx -> Idx -> Idx
toIdxSum = toBinIdxOp Sum

toIdxDiff :: Token -> Idx -> Idx -> Idx
toIdxDiff = toBinIdxOp Diff

toIdxProd :: Token -> Idx -> Idx -> Idx
toIdxProd = toBinIdxOp Prod

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
