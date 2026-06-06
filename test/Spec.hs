import Test.Hspec
import Typecheck(TypeEvaluationError(..),
                TermType(..),
                determineType,
                TypeErrAt)

import Syntax(Identifier, WithContext(..))
import Lexer(alexScanTokens, LineNumber(..))
import Grammar(parseTokens)
import Test.QuickCheck(elements, oneof, forAll, listOf, Gen)
import Test.Hspec.QuickCheck
import Data.Bifunctor (Bifunctor(first))
import Control.Arrow((>>>))
import qualified Data.Map as M
import Control.Monad ((>=>))


-- This represents the possible errors in a metaQasm program, being
-- either an error that occurred when parsing the code or
-- when evaluating the types of the program
data MetaQasmError = ParseError String | TypeErr TypeErrAt deriving (Eq, Show)

type ProgramTypeEvaluationResult = Either MetaQasmError TermType

-- Takes metaQASM code and parses it before checking the
-- type of the program. If it could be parsed and has a valid type,
-- then the type is returned. Otherwise, an error related to either
-- the parsing or type checking of the code is returned.
calcTypeOf :: String -> ProgramTypeEvaluationResult

calcTypeOf = (alexScanTokens >>> parseTokens >>> changeErrTo ParseError) >=> (determineType emptyCtx >>> changeErrTo TypeErr)
  where
    changeErrTo :: (a -> b) -> Either a c -> Either b c
    changeErrTo = first
    emptyCtx = M.empty


-- -- Tests that accessing a register collection that is not in
-- -- the current evaluation scope always fails.
prop_cannotAccessOutOfScopeRegColl :: Identifier -> IO ()

prop_cannotAccessOutOfScopeRegColl regCollName  =
  calcTypeOf registerAccess `shouldBe` variableNotInScopeErr
  where
    registerAccess = regCollName <> "[0]"
    expectedLineNum = LineNumber 1
    variableNotInScopeErr = Left $ TypeErr $ WithContext (VariableNotInScope regCollName) expectedLineNum


outOfScopeRegColl :: Gen String
outOfScopeRegColl = (:) <$> lowerCaseLetter <*> listOf alphaNumeric
  where
    lowerCaseLetter :: Gen Char
    lowerCaseLetter = elements ['a'..'z']

    upperCaseLetter :: Gen Char
    upperCaseLetter = elements ['A'..'Z']

    letter :: Gen Char
    letter = oneof [lowerCaseLetter, upperCaseLetter]

    digit :: Gen Char
    digit = elements ['0'..'9']

    alphaNumeric :: Gen Char
    alphaNumeric = oneof [letter, digit]

main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers that is out of scope" $ do
    prop "Accessing any register returns an error stating the collection is not in scope" $ do
      forAll outOfScopeRegColl prop_cannotAccessOutOfScopeRegColl
