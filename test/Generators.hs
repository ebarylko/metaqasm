{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Generators(outOfScopeRegColl,
                  outOfScopeExpr,
                  programWithQubitInScope,
                  MetaQasmProgram,
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
                 programThatMeasuresABit,
                 programThatStoresQubitMeasurementInAQubit)
  where

import Test.QuickCheck
import Formatting
import Syntax(Identifier,
              NonNeg(..),
              Expression(..),
              WithContext(..),
              TermType(..),
              Id,
              RegisterType(..),
              NonNeg(..))
import Lexer(LineNumber(..))
import Control.Arrow((&&&),
                     (>>>))
import Test.QuickCheck.Instances.Tuple ((>**<), (>*<))
import Data.Function((&), on)
import Typecheck(TypeEvaluationError(..))
import Control.Monad(replicateM)
import Data.List(nub)
import Data.Text.Lazy.Builder(fromString)
import Control.Applicative(liftA3)

builtInGates :: [String]
builtInGates = ["h", "cx", "t", "tdg"]

outOfScopeRegColl :: Gen String
outOfScopeRegColl = ((:) <$> lowerCaseLetter <*> listOf alphaNumeric) `suchThat` (`notElem` builtInGates)
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

-- This type represents the code of a MetaQASM program
type MetaQasmProgram = String

outOfScopeVarName = outOfScopeRegColl

outOfScopeRegAccess :: Gen MetaQasmProgram
outOfScopeRegAccess = (++) <$> outOfScopeRegColl <*> pure "[0]"

outOfScopeExpr :: Gen MetaQasmProgram
outOfScopeExpr = oneof [outOfScopeVarName, outOfScopeRegAccess]

-- This data type represents a request to access a register in a register collection. However, the
-- request can be invalid if the wanted register is outside of the bounds of the collection
data RegCollAccessSpec = RegCollAccessSpec{_regCollName :: Identifier, _numOfRegs :: Int, _wantedRegIdx :: Int}


-- Takes a predicate and returns a generator that only
-- outputs access specifications that satisfy the predicate
genRegCollAccessSpec :: (RegCollAccessSpec -> Bool) -> Gen RegCollAccessSpec

genRegCollAccessSpec predicate = ((>**<) outOfScopeRegColl posNum arbitrarySizedNatural  & fmap (uncurry3 RegCollAccessSpec)) `suchThat` predicate
  where
    posNum :: Gen Int
    posNum = getPositive <$> arbitrary

    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f = \(x, y, z) -> f x y z

-- This generates an instance of a valid register
-- access spec where the wanted register is inside of the
-- register collection
genValidRegCollAccessSpec :: Gen RegCollAccessSpec
genValidRegCollAccessSpec = genRegCollAccessSpec isAccessingValidReg
  where
    isAccessingValidReg (RegCollAccessSpec _ regCount regIdx) = regCount > regIdx

-- This generates an instance of an invalid register
-- access spec where the wanted register is outside of the
-- bounds of the register collection
genInvalidRegCollAccessSpec :: Gen RegCollAccessSpec

genInvalidRegCollAccessSpec = genRegCollAccessSpec isAccessingInvalidReg
  where
    isAccessingInvalidReg (RegCollAccessSpec _ regCount regIdx) = regIdx >= regCount


-- This type represents all formatters that generate a MetaQasm program based off
-- of a specification detailing how to access a register collection
type RegAccessFormatter = Format MetaQasmProgram (RegCollAccessSpec -> MetaQasmProgram)

-- Formats the access of a register in a register collection,
-- generating a string of the form 'regName[regIdx]'
regCollAccess :: RegAccessFormatter
regCollAccess = (accessed _regCollName string) <> squared (accessed _wantedRegIdx int)

-- Takes the type of collection being declared and
-- generates strings of the form "regCollType regCollName[numOfRegs]"
regCollDecl :: String -> RegAccessFormatter
regCollDecl regCollType = toFormatter regCollType  %+  (accessed _regCollName string) <> squared (accessed _numOfRegs int)

-- Takes a name for a quantum register collection, the number of registers in
-- the collection, and generates a string of the form 'qreg collName[numOfRegisters]'
quantumRegCollDecl :: RegAccessFormatter
quantumRegCollDecl = regCollDecl "qreg"

-- Takes a formatter for a declaration and a formatter for an expression
-- evaluated under that declaration, and combines them into a formatter
-- that generates:
--   declaration in { expression }
-- For example:
--   qreg q[2] in { h(q[0]) }
--   gate f(x: Qbit) { h(x) } in { f(q[0]) }
scopedDecl :: Format MetaQasmProgram (a -> MetaQasmProgram) -> Format MetaQasmProgram (a -> MetaQasmProgram) -> Format MetaQasmProgram (a -> MetaQasmProgram)
scopedDecl f g = f %+ "in " <> braced g

-- Takes a formatter for a register access specification and generates a formatter
-- for applying a gate to the accessed qubit/s
appGateToQubits :: RegAccessFormatter -> RegAccessFormatter
appGateToQubits gate = scopedDecl quantumRegCollDecl  gate

toProgWithGateApp :: RegAccessFormatter  -> RegCollAccessSpec -> MetaQasmProgram
toProgWithGateApp = formatToString

toFormatter = now . fromString

singleQubitGateApp :: String -> RegAccessFormatter
singleQubitGateApp gate = toFormatter gate % parenthesised regCollAccess

hadamardApp :: RegAccessFormatter
hadamardApp = singleQubitGateApp "h"

-- Generates metaQASM code where a hadamard gate is applied to
-- a qubit that is in scope
programWithQubitInScope :: Gen MetaQasmProgram

programWithQubitInScope =  toProgWithHGateApp <$> genValidRegCollAccessSpec
  where
    toProgWithHGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithHGateApp =  toProgWithGateApp (appGateToQubits hadamardApp)

-- Generates metaQASM code where an empty
-- register collection is declared
programWithEmptyRegCollDecl :: Gen MetaQasmProgram

programWithEmptyRegCollDecl =  toProgWithEmptyRegCollDecl <$> genInvalidRegCollAccessSpec
  where
    toProgWithEmptyRegCollDecl = toProgWithGateApp (scopedDecl emptyRegCollDecl hadamardApp) 
    emptyRegCollDecl = "qreg" %+ (accessed _regCollName string) % "[0]"

-- Represents pairs of programs and the errors obtained when
-- running them
type ProgramWithExpectedErr = (MetaQasmProgram, TypeEvaluationError)

-- Generate a pair of programs that access invalid registers
-- and the expected register access error received when running them
programWithInvalidRegAccess :: Gen ProgramWithExpectedErr

programWithInvalidRegAccess = genInvalidRegCollAccessSpec & fmap ((&&&) toProgWithInvalidAccess toErr)
  where
    toProgWithInvalidAccess :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithInvalidAccess = toProgWithGateApp (appGateToQubits hadamardApp)

    toErr :: RegCollAccessSpec -> TypeEvaluationError
    toErr (RegCollAccessSpec regCollId _ regIdx') = InvalidRegAccess regCollId (NonNeg regIdx')

tGateApp :: RegAccessFormatter
tGateApp = singleQubitGateApp "t"

-- Generates programs containing the application of a t gate to a qubit
programWithTGateApp :: Gen MetaQasmProgram

programWithTGateApp = toProgWithTGateApp <$> genValidRegCollAccessSpec 
  where
    toProgWithTGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTGateApp = toProgWithGateApp (appGateToQubits tGateApp)

-- Generates programs containing the application of a T dagger gate to a qubit
programWithTDaggerGateApp :: Gen MetaQasmProgram

tDaggerGateApp :: RegAccessFormatter
tDaggerGateApp = singleQubitGateApp "tdg"

programWithTDaggerGateApp = toProgWithTDaggerGateApp <$> genValidRegCollAccessSpec 
  where
    toProgWithTDaggerGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTDaggerGateApp  = toProgWithGateApp (appGateToQubits tDaggerGateApp)



programWithCNotGateApp :: Gen MetaQasmProgram
programWithCNotGateApp  = formatToString toCnotGateApp <$> genValidRegCollAccessSpec
  where
    toCnotGateApp :: RegAccessFormatter
    toCnotGateApp = appGateToQubits cNotGateApp
    cNotGateApp :: RegAccessFormatter
    cNotGateApp = "cx" % parenthesised (regCollAccess % ", " <> regCollAccess)


-- This data type represents the information known about a two qubit gate declaration, namely the name of the
-- gate and the parameters
data TwoQubitGateDeclInfo = TwoQubitGateDeclInfo{_gateName :: String, _fstQubit :: String, _sndQubit :: String}

twoQubitGateDeclInfo :: Gen TwoQubitGateDeclInfo

-- Generates information for a two qubit gate declaration such that the names of the parameters and gate are unique
twoQubitGateDeclInfo = replicateM 3 outOfScopeVarName `suchThat` allNamesAreUnique & fmap (\[x, y, z] -> TwoQubitGateDeclInfo x y z)
  where
    allNamesAreUnique :: [String] -> Bool
    allNamesAreUnique = nub >>> length >>> (== 3)

-- This type represents pairs of specifications about gate declarations and
-- register collection accesses such that the reigster name does not overshadow
-- the gate name or its parameters
type TwoQubitGateDeclAndAppInfo = (TwoQubitGateDeclInfo, RegCollAccessSpec)

-- Generates pairs of gate declaration and register collection access specifications such that
-- the accessed register does not use the same name as the declared gate or its parameters
nonShadowingRegCollAccess :: Gen TwoQubitGateDeclAndAppInfo
nonShadowingRegCollAccess = twoQubitGateDeclInfo >*< genValidRegCollAccessSpec `suchThat` isNotBeingOverShadowedByRegAcc
  where
    isNotBeingOverShadowedByRegAcc  :: (TwoQubitGateDeclInfo, RegCollAccessSpec) -> Bool
    isNotBeingOverShadowedByRegAcc  (TwoQubitGateDeclInfo gateName fstQubitName sndQubitName, RegCollAccessSpec regCollName _ _) = regCollName `notElem` [gateName, fstQubitName, sndQubitName]


-- Generates a two qubit gate declaration that applies a cnot gate to its parameters
toGateDecl :: Format MetaQasmProgram (TwoQubitGateDeclInfo -> MetaQasmProgram)
toGateDecl = "gate" %+ gateName <> parenthesised gateArgs <> " " % braced ("cx" % parenthesised cnotArgs)
  where
    gateName = accessed _gateName string
    fstArg = accessed _fstQubit string
    sndArg = accessed _sndQubit string
    gateArgs = qubitAnnotation fstArg % ", " <>  qubitAnnotation sndArg
    cnotArgs = fstArg % ", " <> sndArg
    qubitAnnotation =  (%+ ": Qbit")

-- Takes a formatter for a gate declaration and a formatter for a gate application and
-- generates a formatter that combines the declaration and application of the gate
fmtGateDeclAndApp :: Format MetaQasmProgram (TwoQubitGateDeclInfo -> MetaQasmProgram) -> RegAccessFormatter -> Format MetaQasmProgram (TwoQubitGateDeclAndAppInfo -> MetaQasmProgram)
fmtGateDeclAndApp gateDeclFormatter gateAppFormatter = scopedDecl fmtGateDecl  fmtGateApp
  where
    fmtGateDecl = accessed fst gateDeclFormatter
    fmtGateApp =  accessed snd $ appGateToQubits gateAppFormatter

-- Generates a MetaQasm program where a two qubit gate
-- is declared and then applied to two in-scope qubits
programWithTwoQubitGateDeclAndApp :: Gen MetaQasmProgram

-- Takes a formatter for a two qubit gate declaration, a formatter for the application of the gate, the information needed for both formatters,
-- and generates a MetaQASM program based on the formatters and info that declares a two qubit gate and applies it later
toTwoQubitGateDeclAndApp :: Format MetaQasmProgram (TwoQubitGateDeclInfo -> MetaQasmProgram) -> (String -> RegAccessFormatter) ->  TwoQubitGateDeclAndAppInfo -> MetaQasmProgram

toTwoQubitGateDeclAndApp gateDeclFormatter gateAppFormatter info@(TwoQubitGateDeclInfo gateName _ _, _) = formatToString (fmtGateDeclAndApp gateDeclFormatter (gateAppFormatter gateName)) info

programWithTwoQubitGateDeclAndApp =  toTwoQubitGateDeclAndApp toGateDecl twoQubitGateApp <$> nonShadowingRegCollAccess
  where
    twoQubitGateApp gate = toFormatter gate %  parenthesised (regCollAccess % ", " <> regCollAccess)

-- Generates programs where a two qubit gate is applied to
-- three qubits
programWithTooManyParamsInGateApp :: Gen MetaQasmProgram

programWithTooManyParamsInGateApp = toTwoQubitGateDeclAndApp toGateDecl threeQubitGateApp <$> nonShadowingRegCollAccess
  where
    threeQubitGateApp gate = toFormatter gate %  parenthesised (regCollAccess % ", " <> regCollAccess % ", " <> regCollAccess)

-- Generates programs where a two qubit gate is applied to
-- one qubit
programWithTooFewParamsInGateApp :: Gen MetaQasmProgram

programWithTooFewParamsInGateApp = toTwoQubitGateDeclAndApp toGateDecl singleQubitGateApp <$> nonShadowingRegCollAccess

-- This type represents the information needed to create a MetaQASMProgram
-- that measures a qubit and stores the measurement in a bit
data QubitMeasurementSpec = QubitMeasurementSpec{quantumRegCollInfo :: RegCollAccessSpec, classicRegCollInfo :: RegCollAccessSpec}

-- Generates pairs of specifications
-- for valid bit and qbit accesses where
-- the names of the accessed collections are unique
qubitMeasurementSpec :: Gen QubitMeasurementSpec

qubitMeasurementSpec = (genValidRegCollAccessSpec >*< genValidRegCollAccessSpec) `suchThat` regCollsHaveUniqueNames & fmap (uncurry QubitMeasurementSpec)
  where
    regCollsHaveUniqueNames :: (RegCollAccessSpec, RegCollAccessSpec) -> Bool
    regCollsHaveUniqueNames  =  uncurry ((/=) `on` _regCollName) 

-- Takes a name for a classic register collection, the number of registers in
-- the collection, and generates a string of the form 'creg collName[numOfRegisters]'
classicRegCollDecl :: RegAccessFormatter
classicRegCollDecl = regCollDecl "creg"

-- This data type represents any formatter that can generate a MetaQASM program
type MetaQasmProgramFormatter a = Format MetaQasmProgram (a -> MetaQasmProgram)

-- Takes two formatters and returns a formatter that measures the value
-- produced by the first formatter and stores the result in the value
-- produced by the second formatter.
formatMeasurement :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a 

formatMeasurement f g = "measure"  %+ f %+ "-> " <> g

-- Generates programs where a qubit is measured and
-- the measurement is stored in a bit
programThatMeasuresAQubit :: Gen MetaQasmProgram

programThatMeasuresAQubit =  toQubitMeasurement <$> qubitMeasurementSpec
  where
    toQubitMeasurement :: QubitMeasurementSpec -> MetaQasmProgram

    toQubitMeasurement = formatToString $ scopedDecl (accessed classicRegCollInfo classicRegCollDecl) $ scopedDecl (accessed quantumRegCollInfo quantumRegCollDecl) measureQubit
    measureQubit = formatMeasurement (accessed quantumRegCollInfo regCollAccess) (accessed classicRegCollInfo regCollAccess)

-- Takes a specification for a valid register access
-- and generates a MetaQASM term corresponding to
-- such an access
toRegAccessOnLine1 :: RegCollAccessSpec -> Expression

toRegAccessOnLine1 RegCollAccessSpec{_regCollName, _wantedRegIdx} =
  RegisterAccess{registerName, registerNumber}
  where
    registerName = WithContext _regCollName line1
    registerNumber = WithContext (NonNeg _wantedRegIdx) line1
    line1 = LineNumber 1

-- Represents pairs of invalid programs and an
-- expression within the program that causes it to
-- be invalid
type InvalidProgram = (MetaQasmProgram, Expression)

-- Given a formatter that generates MetaQASM program that
-- are invalid due to a misplaced bit/qubit, generates pairs
-- of invalid programs and the subexpression responsible for
-- making the program fail
genInvalidProgram :: RegAccessFormatter -> Gen InvalidProgram

genInvalidProgram invalidProgFmtter = (&&&) (formatToString invalidProgFmtter) toRegAccessOnLine1 <$> genValidRegCollAccessSpec

-- Generates pairs of invalid programs that apply a single
-- qubit unitary to a bit and the misplaced bit
programThatAppliesSingleQbitUnitaryToBit :: Gen InvalidProgram

programThatAppliesSingleQbitUnitaryToBit  = genInvalidProgram invalidGateApp
  where
    invalidGateApp = scopedDecl classicRegCollDecl hadamardApp

-- This data type represents invalid MetaQASM programs
-- that treat register collections as if they were gates.
-- It contains the erroneous program, the name of the
-- register collection, and the type of the collection
data InvalidRegCollApp = InvalidRegCollApp{invalidProg :: MetaQasmProgram, regColl :: Id, collType :: TermType} deriving (Show)

-- Takes a description of a valid register access and
-- generates the MetaQASM term corresponding to the
-- collection being accessed
toRegCollOnLine1 :: RegCollAccessSpec -> Id

toRegCollOnLine1 RegCollAccessSpec{_regCollName}  =  WithContext _regCollName (LineNumber 1)

-- Takes the type of the accessed element, a
-- description of a valid register access and
-- generates the type of the register collection being
-- accessed
toRegCollType :: RegisterType -> RegCollAccessSpec -> TermType

toRegCollType collType accessInfo =
  RegisterGroup collType $ WithContext registerCount (LineNumber 1)
  where
    registerCount = (NonNeg . _numOfRegs) accessInfo

toQuantRegColl :: RegCollAccessSpec -> TermType
toQuantRegColl = toRegCollType Quantum

-- Generates information about invalid MetaQASM programs
-- that treats register collections as gates. This information
-- includes said program, the name of the collection, and
-- the type of the collection
programThatTreatsRegCollsAsGates :: Gen InvalidRegCollApp

programThatTreatsRegCollsAsGates  = liftA3 InvalidRegCollApp (formatToString invalidRegCollApp) toRegCollOnLine1 toQuantRegColl <$> genValidRegCollAccessSpec
  where
    invalidRegCollApp = scopedDecl quantumRegCollDecl regCollApp
    regCollApp = accessed _regCollName string  <> parenthesised regCollAccess

-- Generates an erroneous program that
-- measures a bit instead of a qubit
programThatMeasuresABit :: Gen InvalidProgram

programThatMeasuresABit = genInvalidProgram invalidMeasurement
  where
    invalidMeasurement = scopedDecl classicRegCollDecl measureBit
    measureBit = formatMeasurement regCollAccess regCollAccess

-- Generates MetaQASM programs that store the result of
-- measuring a qubit inside of another qubit
programThatStoresQubitMeasurementInAQubit :: Gen InvalidProgram

programThatStoresQubitMeasurementInAQubit = genInvalidProgram invalidMeasurement
  where
    invalidMeasurement = scopedDecl quantumRegCollDecl storeMeasurementInQbit
    storeMeasurementInQbit = formatMeasurement regCollAccess regCollAccess
