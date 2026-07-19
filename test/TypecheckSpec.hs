{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

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
import Generators(outOfScopeVar,
                  outOfScopeExpr,
                  MetaQasmProgram,
                  programWithValidHGateApp,
                  programWithEmptyRegCollDecl,
                  programWithOutOfBoundsRegAccess,
                  ProgramWithExpectedErr,
                  programWithTGateApp,
                  programWithTDaggerGateApp,
                  programWithCNotGateApp,
                  scopedTwoQubitGate,
                  programWithTooManyParamsInGateApp,
                  programWithTooFewParamsInGateApp,
                  programThatMeasuresAQubit,
                  programThatAppliesSingleQbitUnitaryToBit,
                 InvalidProgram,
                 programThatTreatsRegCollsAsGates,
                 InvalidRegCollApp(..),
                 programThatMeasuresABit,
                 programThatStoresQubitMeasurementInAQubit,
                 scopedGateThatAppliesHadamardGateToOneArg,
                 nonscopedRegCollDeclWithHGateApp,
                 nonscopedRegCollDecl,
                 emptyUnscopedRegCollDecl,
                 programThatSequencesEmptyRegCollDecl,
                 programThatSequencesUnscopedClassicRegColl,
                 programThatSequencesUnrelatedCommands,
                 programThatResetsAQubit,
                 programThatResetsABit,
                 unscopedGateDeclAndApp,
                 unscopedTwoQubitGateDecl,
                 multilineUnscopedGateWithQuantumRegCollParam,
                 unscopedGateThatTakesAnEmptyRegColl,
                 gateThatAppliesUnitaryToClassicalRegCollElem,
                 higherOrderedGateDeclAndApp,
                 conditionalGateExecution,
                 programWithGateAppToSubtypeOfExpectedRegColl,
                 programThatSequencesGates)
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

-- -- Tests that accessing a variable that is not in
-- -- the current evaluation scope always fails.
prop_cannotAccessOutOfScopeVar :: Identifier -> IO ()
prop_cannotAccessOutOfScopeVar var  =
  calcTypeOf var `shouldBe` variableNotInScopeErr
  where
    expectedLineNum = LineNumber 1
    variableNotInScopeErr = genNotInScopeErr var expectedLineNum

-- Asserts that a hadamard gate cannot be applied to
-- an out of scope expression
prop_cannotApplyGateToOutOfScopeExpr :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToOutOfScopeExpr expr =
  calcTypeOf hGateApp `shouldBe` variableNotInScopeErr
  where
    hGateApp = formatToString ("h" % parenthesised string ) expr
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

-- Takes a MetaQASM program that applies a register collection
-- to a qubit as if it were a gate, the name of the collection,
-- the type of the collection, and tests that the program is invalid
-- and generates an error noting that the collection should have been a gate
prop_cannotTreatRegCollAsGate :: InvalidRegCollApp -> IO ()

prop_cannotTreatRegCollAsGate InvalidRegCollApp{..} =
  calcTypeOf invalidProg `shouldBe` typeMismatchErr
  where
    typeMismatchErr = Left $ TypeErr $ WithContext (ExpectedAGate collType regColl) (LineNumber 1)

-- Takes the expected type of a term, the actual type of it, a program that applies
-- an invalid operation on said term, and checks that running the program results
-- in an error noting that the term does not have the expected type
prog_cannotSubstituteAForB :: TermType -> TermType -> InvalidProgram -> IO ()

prog_cannotSubstituteAForB expectedType actualType (prog, erroneousTerm) =
  calcTypeOf prog `shouldBe` typeMismatchErr
  where
    typeMismatchErr = Left $ TypeErr $ WithContext (TypeMismatch expectedType actualType erroneousTerm) (LineNumber 1)


prop_cannotSubstituteBitForQubit :: InvalidProgram -> IO ()
prop_cannotSubstituteBitForQubit = prog_cannotSubstituteAForB Qbit Bit

-- Takes a MetaQASM program that applies an operation for bits on a
-- qubit and checks that an error is generated noting this
-- inconsistency
prop_cannotSubstituteQubitForBit :: InvalidProgram -> IO ()
prop_cannotSubstituteQubitForBit = prog_cannotSubstituteAForB Bit Qbit

prop_cannotTakeEmptyRegCollAsArg :: MetaQasmProgram -> IO ()
prop_cannotTakeEmptyRegCollAsArg prog =
  calcTypeOf prog `shouldBe` emptyDeclErr
  where
    emptyDeclErr = Left $ TypeErr $ WithContext (EmptyRegCollDecl regCollName) (LineNumber 1)
    regCollName = extractRegCollName prog
    extractRegCollName = dropWhile isNotPartOfArgList >>> drop 1 >>> takeWhile isBeforeTypeAnnotation
    isNotPartOfArgList = (/= '(')
    isBeforeTypeAnnotation =(/= ':')

spec :: Spec
spec =  do
  describe "Accessing an out of scope variable" $ do
    prop "Is invalid and generates an error" $ do
      forAll outOfScopeVar prop_cannotAccessOutOfScopeVar

  describe "Applying a hadamard gate to an out of scope expression" $ do
    prop "Returns an error stating the expression is not in scope" $ do
      forAll outOfScopeExpr prop_cannotApplyGateToOutOfScopeExpr

  describe "Applying a hadamard gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithValidHGateApp prop_isValidProgram

  describe "Declaring an empty quantum register collection" $ do
    prop "Results in an error noting that this is not permitted" $ do
      forAll programWithEmptyRegCollDecl prop_cannotDeclareEmptyRegColl

  describe "Accessing a register outside the bounds of a register collection" $ do
    prop "Results in an error noting that this is not permitted" $ do
      forAll programWithOutOfBoundsRegAccess prop_cannotAccessRegOutsideOfRegColl

  describe "Applying a t gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithTGateApp prop_isValidProgram

  describe "Applying a t dagger gate to a qubit that is in scope" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithTDaggerGateApp prop_isValidProgram

  describe "Applying a controlled-Not gate to two qubits" $ do
    prop "Is valid and has type unit" $ do
      forAll programWithCNotGateApp prop_isValidProgram

  describe "Declaring a two qubit gate and applying it to two qubits" $ do
    prop "Is valid and has type unit" $ do
      forAll scopedTwoQubitGate prop_isValidProgram

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
      forAll programThatAppliesSingleQbitUnitaryToBit prop_cannotSubstituteBitForQubit

  describe "Treating a register collection as if it were a gate" $ do
    prop "Is invalid and results in an error noting this mismatch" $ do
      forAll programThatTreatsRegCollsAsGates prop_cannotTreatRegCollAsGate

  describe "Trying to measure a bit" $ do
    prop "Is invalid and results in an error noting that a qubit should have been used instead" $ do
      forAll programThatMeasuresABit prop_cannotSubstituteBitForQubit

  describe "Trying to store a qubit measurement inside another qubit" $ do
    prop "Is invalid and results in an error noting that measurements can only be stored in a bit" $ do
      forAll programThatStoresQubitMeasurementInAQubit prop_cannotSubstituteQubitForBit

  describe "Declaring a gate that takes a qubit and a bit and applying it to a qubit and a bit" $ do
    prop "Is valid and has type unit" $ do
      forAll scopedGateThatAppliesHadamardGateToOneArg prop_isValidProgram

  describe "Sequencing a quantum register collection declaration with a Hadamard gate application to one of its elements" $ do
    prop "Is valid and has type unit" $ do
      forAll nonscopedRegCollDeclWithHGateApp prop_isValidProgram

  describe "Declaring a register collection that does not get used" $ do
    prop "Is valid and has type unit" $ do
      forAll nonscopedRegCollDecl prop_isValidProgram

  describe "Declaring an empty unscoped register collection" $ do
    prop "Is invalid" $ do
      forAll emptyUnscopedRegCollDecl prop_cannotDeclareEmptyRegColl

  describe "Sequencing an empty register collection declaration with any other command" $ do
    prop "Is invalid" $ do
      forAll programThatSequencesEmptyRegCollDecl prop_cannotDeclareEmptyRegColl

  describe "Sequencing a classical register collection declaration into a valid command that incorporates it" $ do
    prop "Is valid" $ do
      forAll programThatSequencesUnscopedClassicRegColl prop_isValidProgram

  describe "Sequencing two valid unrelated commands" $ do
    prop "Produces a third valid command" $ do
      forAll programThatSequencesUnrelatedCommands prop_isValidProgram

  describe "Resetting a term that evaluates to a qubit" $ do
    prop "Is valid and has type unit" $ do
      forAll programThatResetsAQubit prop_isValidProgram

  describe "Resetting a term that evaluates to a bit" $ do
    prop "Is invalid" $ do
      forAll programThatResetsABit prop_cannotSubstituteBitForQubit

  describe "Sequencing a valid unscoped gate declaration with its application to the appropriate arguments" $ do
    prop "Is valid" $ do
      forAll unscopedGateDeclAndApp prop_isValidProgram

  describe "Declaring a valid unscoped two qubit gate" $ do
    prop "Is itself valid" $ do
      forAll unscopedTwoQubitGateDecl prop_isValidProgram

  describe "Declaring a multiline unscoped gate that takes a quantum register collection of size N and applying it to such a collection" $ do
    prop "Is valid" $ do
      forAll multilineUnscopedGateWithQuantumRegCollParam prop_isValidProgram

  describe "Declaring an unscoped gate that takes an empty quantum register collection" $ do
    prop "Is invalid" $ do
      forAll unscopedGateThatTakesAnEmptyRegColl prop_cannotTakeEmptyRegCollAsArg

  describe "Declaring a gate that takes an n-sized classical register collection and applying a unitary to an element of it" $ do
    prop "Is invalid" $ do
      forAll gateThatAppliesUnitaryToClassicalRegCollElem prop_cannotSubstituteBitForQubit

  describe "Applying a valid gate that takes a single qubit unitary to the Hadamard gate" $ do
    prop "Is valid" $ do
      forAll higherOrderedGateDeclAndApp prop_isValidProgram

  describe "Applying a valid gate contingent on a valid guard" $ do
    prop "Is itself valid" $ do
      forAll conditionalGateExecution prop_isValidProgram

  describe "Applying a valid gate that takes an N size register collection to a larger register collection" $ do
    prop "Is valid" $ do
      forAll programWithGateAppToSubtypeOfExpectedRegColl prop_isValidProgram

  describe "Sequencing two valid gates" $ do
    prop "Produces a third valid gate" $ do
      forAll programThatSequencesGates prop_isValidProgram
