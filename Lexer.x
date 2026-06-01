{
module Lexer (Token(..), alexMonadScan) where
}

%wrapper "monad"

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
  \[                                                        { token (readBracket TLBracket) }
  \]                                                        { token (readBracket TRBracket) }
  \(                                                        { token (readBracket LParen) }
  \)                                                        { token (readBracket RParen) }
  \"[^\"]*\"                                                { token lexString }
  [a-z]($digit|$alpha)*                                     { token lexId }
  [1-9]$digit*|0                                            { token lexNat }

{

-- OpenQASM tokens
data Token =
  | TLBracket
  | TRBracket
  | TLParen
  | TRParen
  -- identifiers & literals
  | TString String
  | TID String
  | TNat Int
  | EOF
  deriving (Eq,Show)

type TokenGenerator = AlexInput -> Int -> Token

-- Takes the type of expected bracket and returns the corresponding token
readBracket :: Token -> TokenGenerator

readBracket expectedBracket _ _ = expectedBracket

-- Takes an string and generates the corresponding token for it
lexString :: TokenGenerator
lexString (_, _, _, stream) stringLength = TString validId
where validId = filter (/= '"') . take stringLength $ stream

-- Takes an id and generates the corresponding token for it
lexId :: TokenGenerator
lexId (_, _, _, stream) idLength = TID . take idLength $ stream

-- Produces a token for a natural number
lexNat :: TokenGenerator
lexNat (_, _, _, stream) numLength = TNat . num
       where
        num = read . take numLength $ stream



-- Represents the end of a file lexed token
alexEOF :: Alex Token
alexEOF = return EOF

}
