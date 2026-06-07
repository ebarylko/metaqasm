{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Typecheck(TypeEvaluationError(..),
                TermType(..),
                determineType,
                TypeErrAt,
                Term,
                TermType(..))

import Syntax(Identifier, WithContext(..))
import Lexer(alexScanTokens, LineNumber(..))
import Grammar(parseTokens)
import Test.QuickCheck(forAll)
import Test.Hspec.QuickCheck
import Data.Bifunctor (Bifunctor(first))
import Control.Arrow((>>>))
import qualified Data.Map as M
import Control.Monad ((>=>))
import Formatting
import Generators(outOfScopeRegColl,
                  outOfScopeExpr,
                  Expr,
                  programWithQubitInScope)


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

calcTypeOf = parseCode >=> calcType
  where
    changeErrTo :: (a -> b) -> Either a c -> Either b c
    changeErrTo = first
    emptyCtx = M.empty
    parseCode =  alexScanTokens >>> parseTokens >>> changeErrTo ParseError
    calcType = determineType emptyCtx >>> changeErrTo TypeErr


-- Takes the name of a variable not in scope, the line number it was found on,
-- and generates an error stating that the variable on the given line is out
-- of scope
genNotInScopeErr :: Identifier -> LineNumber -> ProgramTypeEvaluationResult
genNotInScopeErr varName lineInfo = Left $ TypeErr $ WithContext (VariableNotInScope varName) lineInfo

-- -- Tests that accessing a register collection that is not in
-- -- the current evaluation scope always fails.
prop_cannotAccessOutOfScopeRegColl :: Identifier -> IO ()

prop_cannotAccessOutOfScopeRegColl regCollName  =
  calcTypeOf registerAccess `shouldBe` variableNotInScopeErr
  where
    registerAccess = regCollName <> "[0]"
    expectedLineNum = LineNumber 1
    variableNotInScopeErr = genNotInScopeErr regCollName expectedLineNum

-- Asserts that a hadamard gate cannot be applied to
-- an out of scope expression
prop_cannotApplyGateToOutOfScopeExpr :: Expr -> IO ()

prop_cannotApplyGateToOutOfScopeExpr expr =
  calcTypeOf hGateApp `shouldBe` variableNotInScopeErr
  where
    hGateApp = formatToString ("h(" % string % ")") expr
    variableNotInScopeErr = genNotInScopeErr (extractVarName expr) (LineNumber 1)
    extractVarName = takeWhile (/= '[')

-- Tests that applying the hadamard gate to qubit
-- that is in scope is a valid operation
prop_canApplyHGateToQbit :: Expr -> IO ()

prop_canApplyHGateToQbit hGateApp =
  calcTypeOf hGateApp `shouldBe` Right Unit

main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers that is out of scope" $ do
    prop "Accessing any register returns an error stating the collection is not in scope" $ do
      forAll outOfScopeRegColl prop_cannotAccessOutOfScopeRegColl

  describe "Applying a hadamard gate to an out of scope expression" $ do
    prop "Returns an error stating the expression is not in scope" $ do
      forAll outOfScopeExpr prop_cannotApplyGateToOutOfScopeExpr

  describe "Applying a hadamard gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithQubitInScope prop_canApplyHGateToQbit
