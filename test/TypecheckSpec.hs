{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module TypecheckSpec(spec) where

import Test.Hspec
import Typecheck(TypeEvaluationError(..),
                determineType,
                CounterExample(..),
                fromEither,
                TypeErrAt,
                Term)

import Syntax(Identifier,
              TermType(..),
              toConstIdx,
              IndexVar(..),
              Index(..),
              WithContext(..))
import Lexer(LineNumber(..))
import Grammar(parseText)
import Test.QuickCheck(forAll)
import Test.Hspec.QuickCheck
import Data.Bifunctor (Bifunctor(first))
import Control.Arrow((>>>))
import qualified Control.Lens as L
import Control.Monad.Except(ExceptT(..), withExceptT, runExceptT)
import qualified Data.Map as M
import Control.Monad ((>=>), liftM2, join)
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
                 InvalidProgCausedByTerm,
                 InvalidProgBcOfTypeAnnotation,
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
                 programThatSequencesGates,
                 programThatAppliesGateToCircSubType,
                 hadamardAppToValidRegAccMadeUsingSumOfIndices,
                 validRegCollDeclUsingSumOfIndices,
                 invalidRegAccessOnGate,
                 emptyRegCollDeclUsingSumOfIndices,
                 validGateThatTakesANonEmptyRegColl,
                 gateThatAppliesHGateToEmptyRegCollElem,
                 programThatAppliesGateToSameSizedRegColl,
                 programThatExecsGateIfBitEqualsSum,
                 programThatAccessesCollWithNegIdx,
                 programThatDeclaresNegLengthColl,
                 gateThatTakesNegLengthColl,
                 gateThatTakesAnInvalidGate,
                 validThirdOrderGateDecl,
                 regAccessedByValidProdOfIndices,
                 validGateDeclThatDoesNotDependOnIndexVars,
                 circuitFamilyThatMayTakeEmptyRegColl,
                 circuitFamilyThatMayTakeNegLengthRegColl,
                 circuitFamilyThatUsesFreeIndexVar,
                 circuitFamilyThatAccessesValidReg,
                 circuitFamilyThatAccessesInvalidReg,
                 circuitFamilyThatTreatsGateAsColl,
                 circuitFamilyWithDuplicateIndexVars,
                 validCircuitFamilyWithGateSequence,
                 circuitFamilyThatUsesFreeIdxVarInBody,
                 circuitFamilyThatAppliesNAryGateToLessThanNArgs,
                 circuitFamilyThatAppliesOneQbitGateToTwoQbits)
import Data.Function(on, (&))

-- This represents the possible errors in a metaQasm program, being
-- either an error that occurred when parsing the code or
-- when evaluating the types of the program
data MetaQasmError = ParseError String | TypeErr TypeErrAt deriving (Eq, Show)

type ProgramTypeEvaluationResult = ExceptT MetaQasmError IO TermType

-- Takes metaQASM code and parses it before checking the
-- type of the program. If it could be parsed and has a valid type,
-- then the type is returned. Otherwise, an error related to either
-- the parsing or type checking of the code is returned.
calcTypeOf :: String -> ProgramTypeEvaluationResult

calcTypeOf = parseCode >=> calcType
  where
    changeErrTo :: (a -> b) -> Either a c -> Either b c
    changeErrTo = first
    parseCode =  parseText >>> changeErrTo ParseError >>> fromEither
    calcType = determineType initialCtx >>> withExceptT TypeErr
    initialCtx = M.fromList [("h", Circuit [Qbit]),
                             ("t", Circuit [Qbit]),
                             ("tdg", Circuit [Qbit]),
                             ("cx", Circuit [Qbit, Qbit])]


-- Takes the name of a variable not in scope, the line number it was found on,
-- and generates an error stating that the variable on the given line is out
-- of scope
genNotInScopeErr :: Identifier -> LineNumber -> ProgramTypeEvaluationResult
genNotInScopeErr varName lineInfo = fromEither $  Left $ TypeErr $ WithContext (VariableNotInScope varName) lineInfo

line1 :: LineNumber
line1 = LineNumber 1

-- Takes a program, the expected type of the program, and returns true if
-- running the program yields the expected type. Throws an error otherwise
shouldHaveType :: MetaQasmProgram -> ProgramTypeEvaluationResult -> IO ()
shouldHaveType prog = (liftM2 shouldBe  `on` runExceptT) (calcTypeOf prog)  >>> join

-- -- Tests that accessing a variable that is not in
-- -- the current evaluation scope always fails.
prop_cannotAccessOutOfScopeVar :: Identifier -> IO ()
prop_cannotAccessOutOfScopeVar var  =
  var `shouldHaveType` variableNotInScopeErr
  where
    expectedLineNum = line1
    variableNotInScopeErr = genNotInScopeErr var expectedLineNum

-- Asserts that a hadamard gate cannot be applied to
-- an out of scope expression
prop_cannotApplyGateToOutOfScopeExpr :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToOutOfScopeExpr expr =
  hGateApp `shouldHaveType` variableNotInScopeErr
  where
    hGateApp = formatToString ("h" % parenthesised string ) expr
    variableNotInScopeErr = genNotInScopeErr (extractVarName expr) line1
    extractVarName = takeWhile (/= '[')

-- Takes a program of the form regType regName[numOfRegs] ... more commands
-- and extracts the name of the declared register collection
getNameFromRegCollDecl :: MetaQasmProgram -> Identifier
getNameFromRegCollDecl = drop 5 >>> takeWhile (/= '[')

-- Takes a function for generating an error about the size of a register collection,
-- a program that declares a register collection of size N, and checks that running
-- the program results in an error about the size of the declared collection
prop_cannotDeclareSizeNRegColl :: (Identifier -> TypeEvaluationError) -> MetaQasmProgram -> IO ()
prop_cannotDeclareSizeNRegColl errFn prog =
  prog `shouldHaveType` nSizedRegCollDeclErr
  where
    nSizedRegCollDeclErr = prog & getNameFromRegCollDecl & errFn & errOnLine1

-- Tests that declaring an empty quantum register
-- collection is an invalid operation
prop_cannotDeclareEmptyRegColl :: MetaQasmProgram -> IO ()
prop_cannotDeclareEmptyRegColl  = prop_cannotDeclareSizeNRegColl EmptyRegCollDecl

-- Takes a MetaQASM program with an invalid register access, the
-- expected error when running the program,
-- and checks that running the program produces the same kind of error
prop_cannotAccessRegOutsideOfRegColl :: ProgramWithExpectedErr -> IO ()
prop_cannotAccessRegOutsideOfRegColl (program, expectedErr) =
  program `shouldHaveType` invalidRegAccessErr
  where
    invalidRegAccessErr = errOnLine1 expectedErr

genExpectedNumOfArgsErr :: Int -> Int -> ProgramTypeEvaluationResult

-- Takes the expected number of arguments to a gate, the actual number of arguments passed, and
-- generates an error noting that the expected and actual number of arguments do not coincide
genExpectedNumOfArgsErr expectedNumOfArgs actualNumOfArgs =
  errOnLine1 $ toUnexpectedNumOfArgsErr expectedNumOfArgs actualNumOfArgs
  where
    toUnexpectedNumOfArgsErr :: Int -> Int -> TypeEvaluationError
    toUnexpectedNumOfArgsErr = ExpectedNParams `on` toConstIdx

-- Checks that a MetaQASM program that applies a two qubit gate
-- to three qubits is invalid
prop_cannotApplyGateToTooManyQubits :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToTooManyQubits  =
   (`shouldHaveType` tooManyArgsErr)
  where
    tooManyArgsErr = genExpectedNumOfArgsErr 2 3

-- Checks that a MetaQASM program that applies a two qubit gate
-- to one qubit is invalid
prop_cannotApplyGateToTooFewQubits :: MetaQasmProgram -> IO ()

prop_cannotApplyGateToTooFewQubits  =
   (`shouldHaveType` tooFewArgsErr)
  where
    tooFewArgsErr = genExpectedNumOfArgsErr 2 1

prop_cannotApplySingleQbitUnitaryToTwoQbits :: MetaQasmProgram -> IO ()
prop_cannotApplySingleQbitUnitaryToTwoQbits =
   (`shouldHaveType` tooFewArgsErr)
  where
    tooFewArgsErr = genExpectedNumOfArgsErr 1 2

-- Checks that running a given MetaQASM program does not produce
-- any errors
prop_isValidProgram :: MetaQasmProgram -> IO ()

prop_isValidProgram = (`shouldHaveType` expectedType)
  where
    expectedType = fromEither $ Right Unit

-- Takes a MetaQASM program that applies a register collection
-- to a qubit as if it were a gate, the name of the collection,
-- the type of the collection, and tests that the program is invalid
-- and generates an error noting that the collection should have been a gate
prop_cannotTreatRegCollAsGate :: InvalidRegCollApp -> IO ()

prop_cannotTreatRegCollAsGate InvalidRegCollApp{..} =
  invalidProg `shouldHaveType` typeMismatchErr
  where
    typeMismatchErr = errOnLine1 $ ExpectedAGate collType regColl

-- Takes the expected type of a term, the actual type of it, a program that applies
-- an invalid operation on said term, and checks that running the program results
-- in an error noting that the term does not have the expected type
prog_cannotSubstituteAForB :: TermType -> TermType -> InvalidProgCausedByTerm -> IO ()

prog_cannotSubstituteAForB expectedType actualType (prog, erroneousTerm) =
  prog `shouldHaveType` typeMismatchErr
  where
    typeMismatchErr = errOnLine1 $ TypeMismatch expectedType actualType erroneousTerm


prop_cannotSubstituteBitForQubit :: InvalidProgCausedByTerm -> IO ()
prop_cannotSubstituteBitForQubit = prog_cannotSubstituteAForB Qbit Bit

-- Takes a MetaQASM program that applies an operation for bits on a
-- qubit and checks that an error is generated noting this
-- inconsistency
prop_cannotSubstituteQubitForBit :: InvalidProgCausedByTerm -> IO ()
prop_cannotSubstituteQubitForBit = prog_cannotSubstituteAForB Bit Qbit

extractNameOfFirstGateArg :: MetaQasmProgram -> Identifier
extractNameOfFirstGateArg = dropWhile isNotPartOfArgList >>> drop 1 >>> takeWhile isBeforeTypeAnnotation
  where
    isNotPartOfArgList = (/= '(')
    isBeforeTypeAnnotation =(/= ':')

-- Takes a function for constructing an error describing the invalid length of a register collection,
-- a program with a gate declaration that takes such an invalid collection as an argument, and tests that
-- running the program results in an error about the length of the collection
prop_cannotTakeInvalidLengthRegCollAsArg :: (Identifier -> TypeEvaluationError) -> MetaQasmProgram -> IO ()
prop_cannotTakeInvalidLengthRegCollAsArg errFn prog =
  prog `shouldHaveType` invalidLengthRegCollErr
  where
    invalidLengthRegCollErr = prog & extractNameOfFirstGateArg & errFn & errOnLine1


prop_cannotTakeEmptyRegCollAsArg :: MetaQasmProgram -> IO ()
prop_cannotTakeEmptyRegCollAsArg = prop_cannotTakeInvalidLengthRegCollAsArg EmptyRegCollDecl

-- Tests that accessing the first element of a single qubit
-- unitary is invalid and results in an error noting this
-- discrepancy
prop_cannotTreatSingleQubitUnitaryAsRegColl :: InvalidProgCausedByTerm -> IO ()

prop_cannotTreatSingleQubitUnitaryAsRegColl (prog, gateName) =
  prog `shouldHaveType` expectedRegCollErr
  where
    expectedRegCollErr = errOnLine1 (ExpectedARegColl gateType gateName)
    gateType = Circuit [Qbit]

errOnLine1 :: TypeEvaluationError -> ProgramTypeEvaluationResult
errOnLine1 = (`WithContext` line1) >>> TypeErr >>> Left >>> fromEither

prop_cannotHaveNegIdx :: ProgramWithExpectedErr -> IO ()
prop_cannotHaveNegIdx (prog, negIdxErr) = prog `shouldHaveType` errOnLine1 negIdxErr

-- Takes a program with a negative length register collection declaration
-- and tests that such a declaration is invalid
prop_cannotDeclNegLengthColl :: MetaQasmProgram -> IO ()
prop_cannotDeclNegLengthColl  = prop_cannotDeclareSizeNRegColl NegSizeRegCollDecl

-- Tests that declaring a gate that takes a negative length
-- register collection is invalid and results in an error
prop_cannotTakeNegLengthCollAsGateArg :: MetaQasmProgram -> IO ()
prop_cannotTakeNegLengthCollAsGateArg = prop_cannotTakeInvalidLengthRegCollAsArg NegSizeRegCollDecl

-- Takes a program with an invalid type annotation, the invalid annotation,
-- and tests that running the program results in an error stating that
-- the annotation is invalid
prop_cannotTakeInvalidCircuitAsArg :: InvalidProgBcOfTypeAnnotation -> IO ()
prop_cannotTakeInvalidCircuitAsArg (prog, invalidTypeAnnot) =
  prog `shouldHaveType` invalidCircErr
  where
    invalidCircErr = InvalidCircuitAnnotation invalidTypeAnnot & errOnLine1

indexVarWithCoeff :: Identifier -> Int ->  Index
indexVarWithCoeff varName coeff = IndexVar varName & flip M.singleton coeff & Index 0

-- Given a circuit family which takes a potentially
-- empty register collection as an argument, checks that
-- such a declaration is invalid and results in an error
-- noting that the collection must be nonempty
prop_cannotTakePotentiallyEmptyRegCollAsArg :: MetaQasmProgram -> IO ()
prop_cannotTakePotentiallyEmptyRegCollAsArg prog =
  prog `shouldHaveType` emptyRegCollErr
  where
    emptyRegCollErr = InvalidParametricRegCollDecl regCollId counterExample & errOnLine1
    counterExample = CounterExample (indexVarWithCoeff "n" 1) (M.singleton (IndexVar "n") 0)
    regCollId = prog & (ignoreIndexVars >>> extractNameOfFirstGateArg)
    ignoreIndexVars = dropWhile (/= ')') >>> drop 1

L.makePrisms ''WithContext
L.makePrisms ''TypeEvaluationError
L.makePrisms ''MetaQasmError

calcTypeOf' :: MetaQasmProgram -> IO (Either MetaQasmError TermType)
calcTypeOf' = runExceptT . calcTypeOf

typeEvaluationErr :: L.Traversal'  (Either MetaQasmError TermType) TypeEvaluationError
typeEvaluationErr = L._Left . _TypeErr . _WithContext . L._1

-- Given a circuit family declaration which takes a
-- register collection that may be of negative length, checks that
-- evaluating the declaration is invalid and yields an error
-- noting this
prop_cannotTakePotentialNegLengthRegCollAsArg :: MetaQasmProgram -> IO ()
prop_cannotTakePotentialNegLengthRegCollAsArg  =
  calcTypeOf'  >=> (`shouldSatisfy` isInvalidLengthCollErr )
  where
    isInvalidLengthCollErr :: Either MetaQasmError TermType -> Bool
    isInvalidLengthCollErr = L.has (typeEvaluationErr . _InvalidParametricRegCollDecl)


-- Takes a program containing a circuit family declaration which
-- uses a free index variable and asserts that the program is
-- invalid
prop_cannotUseFreeIndexVarInCircuitFamDecl :: MetaQasmProgram -> IO ()
prop_cannotUseFreeIndexVarInCircuitFamDecl  = (`shouldHaveType` freeIndexVarUsageErr)
  where
    freeIndexVarUsageErr = UsesFreeIndexVar (indexVarWithCoeff "n" 1)  (IndexVar "n") & errOnLine1

-- Given a circuit family which takes a collection of size n + 1 and
-- attempts to access the (n + 1)th element, checks that such an attempt is
-- invalid and results in an error
prop_cannotPerformInvalidParametricAccess :: MetaQasmProgram -> IO ()
prop_cannotPerformInvalidParametricAccess =
  calcTypeOf'  >=> (`shouldSatisfy` isInvalidParametricAccessErr)
  where
    isInvalidParametricAccessErr :: Either MetaQasmError TermType -> Bool
    isInvalidParametricAccessErr = L.has (typeEvaluationErr . _InvalidParametricRegAcc)

-- Takes a circuit family declaration with duplicate index variables
-- and checks that evaluating it yields an error mentioning the multiplicity
-- of the same index variable
prop_cannotHaveDuplicateIndexVarsInDecl :: MetaQasmProgram -> IO ()
prop_cannotHaveDuplicateIndexVarsInDecl prog =
  prog `shouldHaveType` duplicateIdxVarsErr
  where
    duplicateIdxVarsErr = prog & extractCircname & flip DeclUsesDuplicateIdxVars [IndexVar "n", IndexVar "n"] & errOnLine1
    extractCircname = dropWhile (/= ')') >>> drop 2 >>> takeWhile (/= '(')

-- Takes a circuit family declaration that uses a free index variable
-- in its body and checks that such a declaration is invalid
prop_cannotUseFreeVarInCircuitFamBody :: MetaQasmProgram -> IO ()
prop_cannotUseFreeVarInCircuitFamBody =
  (`shouldHaveType` usedFreeVarErr)
  where
    usedFreeVarErr = UsesFreeIndexVar (indexVarWithCoeff "g" 1) (IndexVar "g") & errOnLine1

-- Takes the name of a property to test, the
-- generator for the data being tested, the property, and
-- tests the property against ten samples of the generator
runPropTenTimes propName generator propertyToTest =
  modifyMaxSuccess (const 10) $ do
  prop propName $ do
    forAll generator propertyToTest

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

  describe "Applying a gate that takes a circuit of type K to a circuit of type K' where K' is a subtype of K" $ do
    prop "Is valid" $ do
      forAll programThatAppliesGateToCircSubType prop_isValidProgram

  describe "Applying a gate that takes a circuit of type K to a circuit of type K' where K' is a subtype of K" $ do
    prop "Is valid" $ do
      forAll programThatAppliesGateToCircSubType prop_isValidProgram

  describe "Applying a hadamard gate to a valid register accessed using a summation of indices" $ do
    prop "Is valid" $ do
      forAll hadamardAppToValidRegAccMadeUsingSumOfIndices prop_isValidProgram

  describe "Declaring a register collection using a sum of indices such that the sum is positive" $ do
    prop "Is valid" $ do
      forAll validRegCollDeclUsingSumOfIndices prop_isValidProgram

  describe "Treating a single qubit unitary as a register collection and attempting to access the first element of it" $ do
    prop "Is invalid" $ do
      forAll invalidRegAccessOnGate prop_cannotTreatSingleQubitUnitaryAsRegColl

  describe "Declaring an empty register collection of size i + i' where i + i = 0"  $ do
    prop "Is invalid" $ do
      forAll emptyRegCollDeclUsingSumOfIndices prop_cannotDeclareEmptyRegColl

  describe "Declaring a gate that takes a nonempty register collection of size i + i which applies an h gate to one of its elements"  $ do
    prop "Is valid" $ do
      forAll validGateThatTakesANonEmptyRegColl prop_isValidProgram

  describe "Declaring a gate that takes an empty register collection of size i + i  which applies an h gate to one of its elements"  $ do
    prop "Is invalid" $ do
      forAll gateThatAppliesHGateToEmptyRegCollElem prop_cannotTakeEmptyRegCollAsArg

  describe "Declaring a valid gate that takes a register collection of size x  + y = n  and applying it to an n sized collection"  $ do
    prop "Is valid" $ do
      forAll programThatAppliesGateToSameSizedRegColl prop_isValidProgram

  describe "Conditionally executing a valid gate if a given bit has the same value as a sum of indices"  $ do
    prop "Is valid" $ do
      forAll programThatExecsGateIfBitEqualsSum prop_isValidProgram

  describe "Accessing a register collection with a negative index"  $ do
    prop "Is invalid" $ do
      forAll programThatAccessesCollWithNegIdx prop_cannotHaveNegIdx

  describe "Declaring a register collection with a negative length"  $ do
    prop "Is invalid" $ do
      forAll programThatDeclaresNegLengthColl prop_cannotDeclNegLengthColl

  describe "Declaring a gate that takes a negative length register collection"  $ do
    prop "Is invalid" $ do
      forAll gateThatTakesNegLengthColl prop_cannotTakeNegLengthCollAsGateArg

  describe "Declaring a gate that takes a circuit which takes a negative length collection"  $ do
    prop "Is invalid" $ do
      forAll gateThatTakesAnInvalidGate prop_cannotTakeInvalidCircuitAsArg

  describe "Declaring a valid third order gate"  $ do
    prop "Is valid" $ do
      forAll validThirdOrderGateDecl prop_isValidProgram


  describe "Accessing the ith element of a register collection of size N where N > i using a product of indices"  $ do
    prop "Is valid" $ do
      forAll regAccessedByValidProdOfIndices prop_isValidProgram

  describe "Declaring a circuit family where the gate declaration does not depend on the index variables and is valid"  $ do
    prop "Is valid" $ do
      forAll validGateDeclThatDoesNotDependOnIndexVars prop_isValidProgram

  describe "Declaring a circuit family where one of the arguments could be an empty collection"  $ do
    runPropTenTimes "Is invalid" circuitFamilyThatMayTakeEmptyRegColl prop_cannotTakePotentiallyEmptyRegCollAsArg

  describe "Declaring a circuit family where one of the arguments could be a collection of negative length"  $ do
    runPropTenTimes "Is invalid"  circuitFamilyThatMayTakeNegLengthRegColl prop_cannotTakePotentialNegLengthRegCollAsArg

  describe "Declaring a circuit family that uses a free index variable"  $ do
    prop "Is invalid" $ do
      forAll circuitFamilyThatUsesFreeIndexVar prop_cannotUseFreeIndexVarInCircuitFamDecl


  describe "Accessing the nth element of a collection of size n + 1"  $ do
    runPropTenTimes "Is valid" circuitFamilyThatAccessesValidReg prop_isValidProgram

  describe "Accessing the (n + 1)th element of a collection of size n + 1"  $ do
    modifyMaxSuccess (const 10) $ do
      prop "Is invalid" $ do
        forAll circuitFamilyThatAccessesInvalidReg prop_cannotPerformInvalidParametricAccess


  describe "Treating a gate like a collection inside a circuit family declaration"  $ do
    modifyMaxSuccess (const 10) $ do
      prop "Is invalid" $ do
        forAll circuitFamilyThatTreatsGateAsColl prop_cannotTreatSingleQubitUnitaryAsRegColl


  describe "Declaring a circuit family with duplicate index variables"  $ do
    prop "Is invalid" $ do
      forAll circuitFamilyWithDuplicateIndexVars prop_cannotHaveDuplicateIndexVarsInDecl


  describe "Declaring a valid circuit family containing a gate sequence"  $ do
    modifyMaxSuccess (const 10) $ do
      prop "Is valid" $ do
        forAll validCircuitFamilyWithGateSequence prop_isValidProgram

  describe "Declaring a circuit family in which a register is accessed using a free index variable"  $ do
    modifyMaxSuccess (const 10) $ do
      prop "Is invalid" $ do
        forAll circuitFamilyThatUsesFreeIdxVarInBody prop_cannotUseFreeVarInCircuitFamBody

  describe "Declaring a circuit family in which an n-ary gate is applied to less than n args"  $ do
    modifyMaxSuccess (const 10) $ do
      prop "Is invalid" $ do
        forAll circuitFamilyThatAppliesNAryGateToLessThanNArgs prop_cannotApplyGateToTooFewQubits


  describe "Declaring a circuit family in which an single qubit unitary is applied to two qubits"  $ do
    modifyMaxSuccess (const 10) $ do
      prop "Is invalid" $ do
        forAll circuitFamilyThatAppliesOneQbitGateToTwoQbits prop_cannotApplySingleQbitUnitaryToTwoQbits
