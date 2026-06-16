{
module Lexer (Token(..), LineNumber(..), alexScanTokens) where
}

%wrapper "posn"

$digit  = 0-9        -- digits
$alpha  = [a-zA-Z]   -- alphabetic characters
$eol    = [\n]       -- newline

tokens :-

  -- Whitespace
  $white+                                                   ;
  $eol                                                      ;

  -- Comments
  \/\/.*                                                    ;

  -- Tokens
  \[                                                        { readBracket LBracket }
  \]                                                        { readBracket RBracket }
  \{                                                        { readBracket LCurlyBracket }
  \}                                                        { readBracket RCurlyBracket }
  \(                                                        { readBracket LParen }
  \)                                                        { readBracket RParen }
  creg                                                       {ignoreInputAndReturn Creg}
  in                                                       {ignoreInputAndReturn In}
  \,                                                       {ignoreInputAndReturn Comma}
  \"[^\"]*\"                                                { lexString }
  [a-z]($digit|$alpha)*                                     { lexId }
  [1-9]$digit*|0                                            { lexNat }

{

newtype LineNumber = LineNumber Int deriving (Eq, Show)

-- OpenQASM tokens
data Token = LBracket LineNumber
  | RBracket LineNumber
  | RCurlyBracket LineNumber
  | LCurlyBracket LineNumber
  | LParen LineNumber
  | RParen LineNumber
  | Str String LineNumber
  | Id String LineNumber
  | Nat Int LineNumber
  | Creg
  | In
  | Comma
  deriving (Eq,Show)

-- Represents functions that takes line information,
-- a portion of the stream to read, and constructs a token
type TokenGenerator = AlexPosn -> String -> Token

type Bracket = LineNumber -> Token

-- Takes the expected bracket and returns the token corresponding to that bracket
readBracket :: Bracket -> TokenGenerator

getLineNumber :: AlexPosn -> LineNumber
getLineNumber (AlexPn _ lineNumber _) = LineNumber lineNumber

readBracket expectedBracket lineInfo _ = (expectedBracket . getLineNumber)  lineInfo

-- Takes a token and returns a function that
-- ignores the current text in the stream and outputs the given
-- token
ignoreInputAndReturn :: Token -> TokenGenerator

ignoreInputAndReturn tok _ _ = tok

-- Takes a function for generating a token given some data and a line number,
-- a function to generate the wanted data, and returns a function that
-- generates a token using the two given functions
genToken :: (a -> LineNumber -> Token) -> (String -> a) -> TokenGenerator

genToken tokFn f = \lineInfo text -> tokFn (f text) (getLineNumber lineInfo)

-- Takes an string and generates the corresponding token for it
lexString = genToken Str (filter (/= '"'))

-- Takes an id and generates the corresponding token for it
lexId = genToken Id id

-- Produces a token for a natural number
lexNat = genToken Nat read

}
