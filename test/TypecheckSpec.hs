{-# LANGUAGE OverloadedStrings #-}
module TypecheckSpec(spec) where

import Test.Hspec
import Typecheck(TypeEvaluationError(..),
                determineType,
                TypeErrAt,
                Term)

import Syntax(Identifier,
              TermType(..),
              WithContext(..),
              NonNeg(..))
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
                  MetaQasmProgram,
                  programWithQubitInScope,
                  programWithEmptyRegCollDecl,
                  programWithInvalidRegAccess,
                  ProgramWithExpectedErr,
                  programWithTGateApp,
                  programWithTDaggerGateApp,
                  programWithCNotGateApp,
                  programWithTwoQubitGateDeclAndApp,
                  programWithTooManyParamsInGateApp,
                  programWithTooFewParamsInGateApp)


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
    parseCode =  alexScanTokens >>> parseTokens >>> changeErrTo ParseError
    calcType = determineType initialCtx >>> changeErrTo TypeErr
    initialCtx = M.fromList [("h", Circuit [Qbit]),
                             ("t", Circuit [Qbit]),
                             ("tdg", Circuit [Qbit]),
                             ("cx", Circuit [Qbit, Qbit])]


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
prop_cannotApplyGateToOutOfScopeExpr :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToOutOfScopeExpr expr =
  calcTypeOf hGateApp `shouldBe` variableNotInScopeErr
  where
    hGateApp = formatToString ("h(" % string % ")") expr
    variableNotInScopeErr = genNotInScopeErr (extractVarName expr) (LineNumber 1)
    extractVarName = takeWhile (/= '[')

-- Tests that declaring an empty quantum register
-- collection is an invalid operation
prop_cannotDeclareEmptyRegColl :: MetaQasmProgram -> IO ()

prop_cannotDeclareEmptyRegColl program =
  calcTypeOf program `shouldBe` emptyRegCollErr
  where
    emptyRegCollErr = Left $ TypeErr $ WithContext (EmptyRegCollDecl regCollName) (LineNumber 1)
    regCollName = extractRegCollName program
    extractRegCollName = drop 5 >>> takeWhile (/= '[')


-- Takes a MetaQASM program with an invalid register access, the
-- expected error when running the program,
-- and checks that running the program produces the same kind of error
prop_cannotAccessRegOutsideOfRegColl :: ProgramWithExpectedErr -> IO ()
prop_cannotAccessRegOutsideOfRegColl (program, expectedErr) =
  calcTypeOf program `shouldBe` invalidRegAccessErr
  where
    invalidRegAccessErr = Left $ TypeErr $ WithContext expectedErr (LineNumber 1)

-- Takes a MetaQasm program with a gate application to a qubit and confirms
-- it has type unit
prop_canApplyGate :: MetaQasmProgram -> IO ()
prop_canApplyGate prog = calcTypeOf prog `shouldBe` Right Unit

-- Checks that a MetaQASM program that applies a two qubit gate
-- to three qubits is invalid
prop_cannotApplyGateToTooManyQubits :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToTooManyQubits prog =
  calcTypeOf prog `shouldBe` tooManyArgsErr
  where
    tooManyArgsErr = Left $ TypeErr $ WithContext ExpectedNParams{expectedNumOfParams = NonNeg 2, actualNumOfParams = NonNeg 3} (LineNumber 1)


-- Checks that a MetaQASM program that applies a two qubit gate
-- to one qubit is invalid
prop_cannotApplyGateToTooFewQubits :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToTooFewQubits prog =
  calcTypeOf prog `shouldBe` tooFewArgsErr
  where
    tooFewArgsErr = Left $ TypeErr $ WithContext ExpectedNParams{expectedNumOfParams = NonNeg 2, actualNumOfParams = NonNeg 1} (LineNumber 1)

spec :: Spec
spec =  do
  describe "Accessing elements from a collection of registers that is out of scope" $ do
    prop "Accessing any register returns an error stating the collection is not in scope" $ do
      forAll outOfScopeRegColl prop_cannotAccessOutOfScopeRegColl

  describe "Applying a hadamard gate to an out of scope expression" $ do
    prop "Returns an error stating the expression is not in scope" $ do
      forAll outOfScopeExpr prop_cannotApplyGateToOutOfScopeExpr

  describe "Applying a hadamard gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithQubitInScope prop_canApplyGate

  describe "Declaring an empty quantum register collection" $ do
    prop "Results in an error noting that this is not permitted" $ do
      forAll programWithEmptyRegCollDecl prop_cannotDeclareEmptyRegColl

  describe "Accessing a register outside the bounds of a register collection" $ do
    prop "Results in an error noting that this is not permitted" $ do
      forAll programWithInvalidRegAccess prop_cannotAccessRegOutsideOfRegColl

  describe "Applying a t gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithTGateApp prop_canApplyGate

  describe "Applying a t dagger gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithTDaggerGateApp prop_canApplyGate

  describe "Applying a controlled-Not gate to two qubits" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithCNotGateApp prop_canApplyGate

  describe "Declaring a two qubit gate and applying it to two qubits" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithTwoQubitGateDeclAndApp prop_canApplyGate

  describe "Declaring a two qubit gate and applying it to three qubits" $ do
    prop "Is invalid and generates an error noting this discrepancy" $ do
      forAll programWithTooManyParamsInGateApp prop_cannotApplyGateToTooManyQubits

  describe "Declaring a two qubit gate and applying it to one qubit" $ do
    prop "Is invalid and generates an error noting this discrepancy" $ do
      forAll programWithTooFewParamsInGateApp prop_cannotApplyGateToTooFewQubits
