{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Generators(freshVariable,
                  outOfScopeExpr,
                  programWithValidHGateApp,
                  MetaQasmProgram,
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
                 programThatSequencesUnscopedClassicRegColl)
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

freshVariable :: Gen String
freshVariable = ((:) <$> lowerCaseLetter <*> listOf alphaNumeric) `suchThat` (`notElem` builtInGates)
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

outOfScopeVarName = freshVariable

outOfScopeRegAccess :: Gen MetaQasmProgram
outOfScopeRegAccess = (++) <$> freshVariable <*> pure "[0]"

outOfScopeExpr :: Gen MetaQasmProgram
outOfScopeExpr = oneof [outOfScopeVarName, outOfScopeRegAccess]

-- This data type represents a request to access a register in a register collection. However, the
-- request can be invalid if the wanted register is outside of the bounds of the collection
data RegCollAccessSpec = RegCollAccessSpec{_regCollName :: Identifier, _numOfRegs :: Int, _wantedRegIdx :: Int}


-- Takes a predicate and returns a generator that only
-- outputs access specifications that satisfy the predicate
genRegCollAccessSpec :: (RegCollAccessSpec -> Bool) -> Gen RegCollAccessSpec

genRegCollAccessSpec predicate = ((>**<) freshVariable posNum arbitrarySizedNatural  & fmap (uncurry3 RegCollAccessSpec)) `suchThat` predicate
  where
    posNum :: Gen Int
    posNum = getPositive <$> arbitrary

    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f = \(x, y, z) -> f x y z

-- This generates an instance of a valid register
-- access spec where the wanted register is inside of the
-- register collection
validRegCollAccess :: Gen RegCollAccessSpec
validRegCollAccess = genRegCollAccessSpec isAccessingValidReg
  where
    isAccessingValidReg (RegCollAccessSpec _ regCount regIdx) = regCount > regIdx

-- This generates an instance of an invalid register
-- access spec where the wanted register is outside of the
-- bounds of the register collection
invalidRegCollAccess :: Gen RegCollAccessSpec

invalidRegCollAccess = genRegCollAccessSpec isAccessingInvalidReg
  where
    isAccessingInvalidReg (RegCollAccessSpec _ regCount regIdx) = regIdx >= regCount


-- This type represents all formatters that generate a MetaQasm program based off
-- of a specification detailing how to access a register collection
type RegAccessFormatter = Format MetaQasmProgram (RegCollAccessSpec -> MetaQasmProgram)

-- Formats the access of a register in a register collection,
-- generating a string of the form 'regName[regIdx]'
regCollAccess :: RegAccessFormatter
regCollAccess = (accessed _regCollName string) <> squared (accessed _wantedRegIdx int)

toFormatter :: String -> Format r (a -> r)
toFormatter = fconst . fromString

-- Takes the type of collection being declared and
-- generates strings of the form "regCollType regCollName[numOfRegs]"
regCollDecl :: String -> RegAccessFormatter
regCollDecl regCollType = toFormatter regCollType  <%+>  (accessed _regCollName string) <> squared (accessed _numOfRegs int)

-- Takes a name for a quantum register collection, the number of registers in
-- the collection, and generates a string of the form 'qreg collName[numOfRegisters]'
quantumRegCollDecl :: RegAccessFormatter
quantumRegCollDecl = regCollDecl "qreg"

-- This data type represents any formatter that can generate a MetaQASM program
type MetaQasmProgramFormatter a = Format MetaQasmProgram (a -> MetaQasmProgram)

-- Takes a formatter for a declaration and a formatter for an expression
-- evaluated under that declaration, and combines them into a formatter
-- that generates:
--   declaration in { expression }
-- For example:
--   qreg q[2] in { h(q[0]) }
--   gate f(x: Qbit) { h(x) } in { f(q[0]) }
scopedDecl :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
scopedDecl f g = f <%+> fconst "in" <%+> braced g

-- Takes a formatter for a register access specification and generates a formatter
-- for applying a gate to the accessed qubit/s
appGateToQubits :: RegAccessFormatter -> RegAccessFormatter
appGateToQubits gate = scopedDecl quantumRegCollDecl  gate


singleQubitGateApp :: String -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
singleQubitGateApp gateName gateArg = toFormatter gateName <> parenthesised gateArg

hadamardApp :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
hadamardApp = singleQubitGateApp "h"

hadamardApp' :: RegAccessFormatter
hadamardApp' = singleQubitGateApp "h" regCollAccess


-- Generates metaQASM code where a hadamard gate is applied to
-- a qubit that is in scope
programWithValidHGateApp :: Gen MetaQasmProgram
programWithValidHGateApp =  toProgWithHGateApp <$> validRegCollAccess
  where
    toProgWithHGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithHGateApp =  formatToString (appGateToQubits hadamardApp')

emptyRegCollDecl :: RegAccessFormatter

emptyRegCollDecl = fconst "qreg" <%+> accessed _regCollName string <> fconst "[0]"

-- Generates metaQASM code where an empty
-- register collection is declared
programWithEmptyRegCollDecl :: Gen MetaQasmProgram

programWithEmptyRegCollDecl =  toProgWithEmptyRegCollDecl <$> invalidRegCollAccess
  where
    toProgWithEmptyRegCollDecl = formatToString (scopedDecl emptyRegCollDecl hadamardApp')

-- Represents pairs of programs and the errors obtained when
-- running them
type ProgramWithExpectedErr = (MetaQasmProgram, TypeEvaluationError)

-- Generate a pair of programs that access invalid registers
-- and the expected register access error received when running them
programWithOutOfBoundsRegAccess :: Gen ProgramWithExpectedErr
programWithOutOfBoundsRegAccess = invalidRegCollAccess & fmap ((&&&) toProgWithInvalidAccess toErr)
  where
    toProgWithInvalidAccess :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithInvalidAccess = formatToString (appGateToQubits hadamardApp')

    toErr :: RegCollAccessSpec -> TypeEvaluationError
    toErr (RegCollAccessSpec regCollId _ regIdx') = InvalidRegAccess regCollId (NonNeg regIdx')

tGateApp :: RegAccessFormatter
tGateApp = singleQubitGateApp "t" regCollAccess

-- Generates programs containing the application of a t gate to a qubit
programWithTGateApp :: Gen MetaQasmProgram

programWithTGateApp = toProgWithTGateApp <$> validRegCollAccess 
  where
    toProgWithTGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTGateApp = formatToString (appGateToQubits tGateApp)

-- Generates programs containing the application of a T dagger gate to a qubit
programWithTDaggerGateApp :: Gen MetaQasmProgram

tDaggerGateApp :: RegAccessFormatter
tDaggerGateApp = singleQubitGateApp "tdg" regCollAccess

programWithTDaggerGateApp = toProgWithTDaggerGateApp <$> validRegCollAccess
  where
    toProgWithTDaggerGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTDaggerGateApp  = formatToString (appGateToQubits tDaggerGateApp)

-- Takes two formatters and applies a cnot gate to the values obtained by the formatters
-- Ex: cnot (fconst "x") (fconst "y") = "cx(x, y)"
cnot :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
cnot = twoParamGateApp (fconst "cx")


programWithCNotGateApp :: Gen MetaQasmProgram
programWithCNotGateApp  = formatToString toCnotGateApp <$> validRegCollAccess
  where
    toCnotGateApp :: RegAccessFormatter
    toCnotGateApp = appGateToQubits (cnot regCollAccess regCollAccess)


-- This data type represents the information known about a two arg gate declaration, namely the name of the
-- gate and the parameters
data TwoArgGateDeclInfo = TwoArgGateDeclInfo{_gateName :: String, _fstArg:: String, _sndArg :: String}

twoArgGateDeclInfo :: Gen TwoArgGateDeclInfo

-- Generates information for a two qubit gate declaration such that the names of the parameters and gate are unique
twoArgGateDeclInfo = replicateM 3 outOfScopeVarName `suchThat` allNamesAreUnique & fmap (\[x, y, z] -> TwoArgGateDeclInfo x y z)
  where
    allNamesAreUnique :: [String] -> Bool
    allNamesAreUnique = nub >>> length >>> (== 3)

-- This type represents pairs of specifications about gate declarations and
-- register collection accesses such that the reigster name does not overshadow
-- the gate name or its parameters
type TwoQubitGateDeclAndAppInfo = (TwoArgGateDeclInfo, RegCollAccessSpec)

-- Generates pairs of gate declaration and register collection access specifications such that
-- the accessed register collection does not use the same name as the declared gate
nonShadowingRegCollAccess :: Gen TwoQubitGateDeclAndAppInfo
nonShadowingRegCollAccess = twoArgGateDeclInfo >*< validRegCollAccess `suchThat` isNotBeingOverShadowedByRegAcc
  where
    isNotBeingOverShadowedByRegAcc  :: (TwoArgGateDeclInfo, RegCollAccessSpec) -> Bool
    isNotBeingOverShadowedByRegAcc  (TwoArgGateDeclInfo gateName _ _, RegCollAccessSpec regCollName _ _) = regCollName /= gateName

qubitAnnotation = later (<> ": Qbit")
bitAnnotation = later (<> ": Bit")

fstParam :: MetaQasmProgramFormatter TwoArgGateDeclInfo

fstParam = accessed _fstArg string

sndParam :: MetaQasmProgramFormatter  TwoArgGateDeclInfo
sndParam = accessed _sndArg string

-- Generates a two qubit gate declaration that applies a cnot gate to its parameters
twoQubitGateDecl :: Format MetaQasmProgram (TwoArgGateDeclInfo -> MetaQasmProgram)
twoQubitGateDecl = gateDecl (qubitAnnotation %. fstParam) (qubitAnnotation %. sndParam) (cnot fstParam sndParam)

-- Generates a MetaQasm program where a two qubit gate
-- is declared and then applied to two in-scope qubits
scopedTwoQubitGate :: Gen MetaQasmProgram

-- Takes a formatter for a two qubit gate declaration, a formatter for the application of the gate, the information needed for both formatters,
-- and generates a MetaQASM program based on the formatters and info that declares a two qubit gate and applies it later
toTwoQubitGateDeclAndApp :: Format MetaQasmProgram (TwoArgGateDeclInfo -> MetaQasmProgram) -> (String -> RegAccessFormatter) ->  TwoQubitGateDeclAndAppInfo -> MetaQasmProgram

toTwoQubitGateDeclAndApp gateDeclFormatter gateAppFormatter info@(TwoArgGateDeclInfo gateName _ _, _) = formatToString (fmtGateDeclAndApp gateDeclFormatter (gateAppFormatter gateName)) info
  where
    fmtGateDeclAndApp :: Format MetaQasmProgram (TwoArgGateDeclInfo -> MetaQasmProgram) -> RegAccessFormatter -> Format MetaQasmProgram (TwoQubitGateDeclAndAppInfo -> MetaQasmProgram)
    fmtGateDeclAndApp gateDeclFormatter' gateAppFormatter' = scopedDecl (accessed fst gateDeclFormatter')  (accessed snd $ appGateToQubits gateAppFormatter')

scopedTwoQubitGate =  toTwoQubitGateDeclAndApp twoQubitGateDecl twoQubitGateApp <$> nonShadowingRegCollAccess
  where
    twoQubitGateApp gate = twoParamGateApp (toFormatter gate)  regCollAccess regCollAccess

-- Generates programs where a two qubit gate is applied to
-- three qubits
programWithTooManyParamsInGateApp :: Gen MetaQasmProgram

programWithTooManyParamsInGateApp = toTwoQubitGateDeclAndApp twoQubitGateDecl threeQubitGateApp <$> nonShadowingRegCollAccess
  where
    threeQubitGateApp gate = toFormatter gate <>  parenthesised (regCollAccess <> comma <%+> regCollAccess <> comma <%+> regCollAccess)

-- Generates programs where a two qubit gate is applied to
-- one qubit
programWithTooFewParamsInGateApp :: Gen MetaQasmProgram

programWithTooFewParamsInGateApp = toTwoQubitGateDeclAndApp twoQubitGateDecl (flip singleQubitGateApp regCollAccess) <$> nonShadowingRegCollAccess

-- This type represents the information needed to create a MetaQASMProgram
-- that measures a qubit and stores the measurement in a bit
data QubitMeasurementSpec = QubitMeasurementSpec{quantumRegCollInfo :: RegCollAccessSpec, classicRegCollInfo :: RegCollAccessSpec}

-- Generates pairs of specifications
-- for valid bit and qbit accesses where
-- the names of the accessed collections are unique
qubitMeasurementSpec :: Gen QubitMeasurementSpec

qubitMeasurementSpec = (validRegCollAccess >*< validRegCollAccess) `suchThat` regCollsHaveUniqueNames & fmap (uncurry QubitMeasurementSpec)
  where
    regCollsHaveUniqueNames :: (RegCollAccessSpec, RegCollAccessSpec) -> Bool
    regCollsHaveUniqueNames  =  uncurry ((/=) `on` _regCollName)

-- Takes a name for a classic register collection, the number of registers in
-- the collection, and generates a string of the form 'creg collName[numOfRegisters]'
classicRegCollDecl :: RegAccessFormatter
classicRegCollDecl = regCollDecl "creg"


-- Takes two formatters and returns a formatter that measures the value
-- produced by the first formatter and stores the result in the value
-- produced by the second formatter.
-- Ex: if the first formatter produces "x" and the second produces "y",
-- what is generated is "measure x -> y"
formatMeasurement :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a

formatMeasurement f g = fconst "measure"  <%+> f <%+> fconst "->" <%+> g

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

-- Given a formatter that generates MetaQASM programs that
-- are invalid due to a misplaced bit/qubit, generates pairs
-- of invalid programs and the bit/qubit responsible for
-- making the program fail
genInvalidProgram :: RegAccessFormatter -> Gen InvalidProgram

genInvalidProgram invalidProgFmtter = (&&&) (formatToString invalidProgFmtter) toRegAccessOnLine1 <$> validRegCollAccess

-- Generates pairs of invalid programs that apply a single
-- qubit unitary to a bit and the misplaced bit
programThatAppliesSingleQbitUnitaryToBit :: Gen InvalidProgram

programThatAppliesSingleQbitUnitaryToBit  = genInvalidProgram invalidGateApp
  where
    invalidGateApp = scopedDecl classicRegCollDecl hadamardApp'

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

programThatTreatsRegCollsAsGates  = liftA3 InvalidRegCollApp (formatToString invalidRegCollApp) toRegCollOnLine1 toQuantRegColl <$> validRegCollAccess
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

-- This data type represents the information needed to construct a scoped gate that
-- takes a qubit and bit
data GateThatTakesQubitAndBit = GateThatTakesQubitAndBit{_gateInfo :: TwoArgGateDeclInfo, _measurementComponents :: QubitMeasurementSpec}

gateThatTakesQubitAndBit :: Gen GateThatTakesQubitAndBit

gateThatTakesQubitAndBit = uncurry GateThatTakesQubitAndBit <$> (twoArgGateDeclInfo >*< qubitMeasurementSpec ) `suchThat` gateDoesNotOvershadowRegColls
  where
    gateDoesNotOvershadowRegColls :: (TwoArgGateDeclInfo, QubitMeasurementSpec) -> Bool
    gateDoesNotOvershadowRegColls (declSpec, measurementInfo) = _gateName declSpec `notElem` [getQuantumRegCollName measurementInfo, getClassicalRegCollName measurementInfo]
    getQuantumRegCollName = _regCollName . quantumRegCollInfo
    getClassicalRegCollName = _regCollName . classicRegCollInfo


comma :: Format r (a -> r)
comma = fconst ","

-- Takes two formatters for the types of the arguments to the gate, a formatter for the gate body, and
-- returns a formatter that generates a two qubit gate declaration with the argument types dictated by
-- the first formatter and the body by the other
gateDecl :: MetaQasmProgramFormatter TwoArgGateDeclInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo
gateDecl fstArgFormatter sndArgFormatter gateBodyFormatter  =  (fconst "gate") <%+> (accessed _gateName string) <> parenthesised (fstArgFormatter <> comma <%+> sndArgFormatter) <%+> braced gateBodyFormatter

twoParamGateApp :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a

twoParamGateApp gateNameFormatter fstArgFormatter sndArgFormatter = gateNameFormatter <> parenthesised (fstArgFormatter <> comma <%+> sndArgFormatter)

-- Generates a declaration for a gate that takes a qubit
-- and a bit and applies a hadamard gate to the qubit
scopedGateThatAppliesHadamardGateToOneArg :: Gen MetaQasmProgram

scopedGateThatAppliesHadamardGateToOneArg = formatToString scopedGate <$> gateThatTakesQubitAndBit
  where
    scopedGate = scopedDecl qregColl $ scopedDecl cregColl $ scopedDecl gate gateApp
    qregColl = accessed quantumMeasurementComponent quantumRegCollDecl
    cregColl = accessed classicalMeasurementComponent classicRegCollDecl
    gate = accessed _gateInfo gateDecl'
    gateDecl' = gateDecl (qubitAnnotation %. fstParam) (bitAnnotation %. sndParam) (hadamardApp fstParam)
    gateApp = twoParamGateApp (accessed (_gateName . _gateInfo) string) qubit bit
    qubit = accessed quantumMeasurementComponent regCollAccess
    bit = accessed classicalMeasurementComponent regCollAccess
    quantumMeasurementComponent = quantumRegCollInfo . _measurementComponents
    classicalMeasurementComponent = classicRegCollInfo . _measurementComponents


sepBySemicolon :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a
sepBySemicolon f g = f <> semicolon <%+> g
  where
    semicolon :: MetaQasmProgramFormatter a
    semicolon = fconst ";"

-- Generates a program that declares a quantum register collection
-- before applying a Hadamard gate to a qubit in the collection
nonscopedRegCollDeclWithHGateApp :: Gen MetaQasmProgram
nonscopedRegCollDeclWithHGateApp = formatToString (quantumRegCollDecl `sepBySemicolon` hadamardApp') <$> validRegCollAccess

-- Generates a program consisting solely of an
-- unscoped register collection declaration
nonscopedRegCollDecl :: Gen MetaQasmProgram

nonscopedRegCollDecl = formatToString quantumRegCollDecl <$> validRegCollAccess

-- Generates a program only containing an
-- unscoped empty register collection declaration
emptyUnscopedRegCollDecl :: Gen MetaQasmProgram

emptyUnscopedRegCollDecl = formatToString emptyRegCollDecl <$> validRegCollAccess

-- Generates a program that declares an empty quantum register collection
-- before applying a Hadamard gate to a qubit in the collection
programThatSequencesEmptyRegCollDecl :: Gen MetaQasmProgram
programThatSequencesEmptyRegCollDecl = formatToString (emptyRegCollDecl `sepBySemicolon` hadamardApp') <$> validRegCollAccess

-- Generates a program that first declares a classic register collection
-- before sequencing it with a command that uses it
-- programThatSequencesUnscopedClassicRegColl
programThatSequencesUnscopedClassicRegColl :: Gen MetaQasmProgram
programThatSequencesUnscopedClassicRegColl = formatToString (classicRegCollDecl' `sepBySemicolon`  quantumRegCollDecl' `sepBySemicolon`  formatMeasurement qubit' bit') <$>  qubitMeasurementSpec
  where
    quantumRegCollDecl' = accessed quantumRegCollInfo quantumRegCollDecl
    classicRegCollDecl' = accessed classicRegCollInfo classicRegCollDecl
    qubit' = accessed quantumRegCollInfo regCollAccess
    bit' = accessed classicRegCollInfo regCollAccess
