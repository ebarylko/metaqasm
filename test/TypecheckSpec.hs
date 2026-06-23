{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Lexer(LineNumber(..))
import Grammar(parseText)
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
                  programWithTooFewParamsInGateApp,
                  programThatMeasuresAQubit,
                  programThatAppliesSingleQbitUnitaryToBit,
                 InvalidProgram,
                 programThatTreatsRegCollsAsGates,
                 InvalidRegCollApp(..),
                 programThatMeasuresABit)
import Data.Function(on)

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
    parseCode =  parseText >>> changeErrTo ParseError
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

genExpectedNumOfArgsErr :: Int -> Int -> ProgramTypeEvaluationResult

-- Takes the expected number of arguments to a gate, the actual number of arguments passed, and
-- generates an error noting that the expected and actual number of arguments do not coincide
genExpectedNumOfArgsErr expectedNumOfArgs actualNumOfArgs =
  Left $ TypeErr $ WithContext (toUnexpectedNumOfArgsErr expectedNumOfArgs actualNumOfArgs) (LineNumber 1)
  where
    toUnexpectedNumOfArgsErr :: Int -> Int -> TypeEvaluationError
    toUnexpectedNumOfArgsErr = ExpectedNParams `on` NonNeg

-- Checks that a MetaQASM program that applies a two qubit gate
-- to three qubits is invalid
prop_cannotApplyGateToTooManyQubits :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToTooManyQubits prog =
  calcTypeOf prog `shouldBe` tooManyArgsErr
  where
    tooManyArgsErr = genExpectedNumOfArgsErr 2 3


-- Checks that a MetaQASM program that applies a two qubit gate
-- to one qubit is invalid
prop_cannotApplyGateToTooFewQubits :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToTooFewQubits prog =
  calcTypeOf prog `shouldBe` tooFewArgsErr
  where
    tooFewArgsErr = genExpectedNumOfArgsErr 2 1

-- Checks that running a given MetaQASM program does not produce
-- any errors
prop_isValidProgram :: MetaQasmProgram -> IO ()

prop_isValidProgram prog = calcTypeOf prog `shouldBe` Right Unit

-- Takes a MetaQASM program that applies a single qubit
-- gate to a bit, the bit being evaluated in the program,
-- and checks that the program is invalid and generates an error
-- noting that the bit should have been a qubit
prop_cannotApplyGateToBit :: InvalidProgram -> IO ()

prop_cannotApplyGateToBit (prog, misplacedBit) =
  calcTypeOf prog `shouldBe` typeMismatchErr
  where
    typeMismatchErr = Left $ TypeErr $ WithContext (TypeMismatch expectedType actualType misplacedBit) (LineNumber 1)
    expectedType = Qbit
    actualType = Bit

-- Takes a MetaQASM program that applies a register collection
-- to a qubit as if it were a gate, the name of the collection,
-- the type of the collection, and tests that the program is invalid
-- and generates an error noting that the collection should have been a gate
prop_cannotTreatRegCollAsGate :: InvalidRegCollApp -> IO ()

prop_cannotTreatRegCollAsGate InvalidRegCollApp{invalidProg, regColl, collType} =
  calcTypeOf invalidProg `shouldBe` typeMismatchErr
  where
    typeMismatchErr = Left $ TypeErr $ WithContext (ExpectedAGate collType regColl) (LineNumber 1)

-- Takes a program that mistakingly measures a bit
-- and checks that an error is generated stating that
-- a qubit should have been measured
prop_cannotMeasureBit :: InvalidProgram -> IO ()

prop_cannotMeasureBit (prog, misplacedBit) =
  calcTypeOf prog `shouldBe` mismatchErr
  where
    mismatchErr = Left $ TypeErr $ WithContext (TypeMismatch expectedType actualType misplacedBit) (LineNumber 1)
    expectedType = Qbit
    actualType = Bit


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

  describe "Measuring a qubit and storing the result in a bit" $ do
    prop "Is valid and has type unit" $ do
      forAll programThatMeasuresAQubit prop_isValidProgram

  describe "Applying a single qubit gate to a bit" $ do
    prop "Is invalid and results in an error noting this mismatch" $ do
      forAll programThatAppliesSingleQbitUnitaryToBit prop_cannotApplyGateToBit

  describe "Treating a register collection as if it were a gate" $ do
    prop "Is invalid and results in an error noting this mismatch" $ do
      forAll programThatTreatsRegCollsAsGates prop_cannotTreatRegCollAsGate

  describe "Trying to measure a bit" $ do
    prop "Is invalid and results in an error noting that a qubit should have been used instead" $ do
      forAll programThatMeasuresABit prop_cannotMeasureBit
