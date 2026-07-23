{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Generators(outOfScopeVar,
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
                 programThatAppliesGateToCircSubType)
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
import Data.Function(on)
import Typecheck(TypeEvaluationError(..))
import Control.Monad(replicateM)
import Data.List(nub, (\\))
import Data.Text.Lazy.Builder(fromString)
import Control.Applicative(liftA3)
import Control.Lens hiding (elements)

reservedKeywords :: [String]
reservedKeywords = ["h", "cx", "t", "tdg", "in", "if"]

freshVariable :: Gen String
freshVariable = ((:) <$> lowerCaseLetter <*> listOf alphaNumeric) `suchThat` (`notElem` reservedKeywords)
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

outOfScopeVar :: Gen MetaQasmProgram
outOfScopeVar = freshVariable

outOfScopeRegAccess :: Gen MetaQasmProgram
outOfScopeRegAccess = (++) <$> freshVariable <*> pure "[0]"

outOfScopeExpr :: Gen MetaQasmProgram
outOfScopeExpr = oneof [freshVariable, outOfScopeRegAccess]

-- This data type represents a request to access a register in a register collection. However, the
-- request can be invalid if the wanted register is outside of the bounds of the collection
data RegCollAccessSpec = RegCollAccessSpec{_regCollName :: Identifier, _numOfRegs :: Int, _wantedRegIdx :: Int} deriving (Show)
makeLenses ''RegCollAccessSpec

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(x, y, z) -> f x y z

-- Takes a predicate and returns a generator that only
-- outputs access specifications that satisfy the predicate
genRegCollAccessSpec :: (RegCollAccessSpec -> Bool) -> Gen RegCollAccessSpec

genRegCollAccessSpec predicate = ((>**<) freshVariable posNum arbitrarySizedNatural  & fmap (uncurry3 RegCollAccessSpec)) `suchThat` predicate
  where
    posNum :: Gen Int
    posNum = getPositive <$> arbitrary


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
regCollAccess = (viewed regCollName string) <> squared (viewed wantedRegIdx int)

toFormatter :: String -> Format r (a -> r)
toFormatter = fconst . fromString

-- Takes the type of collection being declared and
-- generates strings of the form "regCollType regCollName[numOfRegs]"
regCollDecl :: String -> RegAccessFormatter
regCollDecl regCollType = toFormatter regCollType  <%+>  (viewed regCollName string) <> squared (viewed numOfRegs int)

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


singleParamGateApp :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
singleParamGateApp gateName gateArg = gateName <> parenthesised gateArg

hadamardApp :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
hadamardApp = singleParamGateApp (fconst "h")

hadamardApp' :: RegAccessFormatter
hadamardApp' = singleParamGateApp (fconst "h") regCollAccess


-- Generates metaQASM code where a hadamard gate is applied to
-- a qubit that is in scope
programWithValidHGateApp :: Gen MetaQasmProgram
programWithValidHGateApp =  toProgWithHGateApp <$> validRegCollAccess
  where
    toProgWithHGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithHGateApp =  formatToString (appGateToQubits hadamardApp')

emptyRegCollDecl :: RegAccessFormatter

emptyRegCollDecl = fconst "qreg" <%+> viewed regCollName string <> fconst "[0]"

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
tGateApp = singleParamGateApp (fconst "t") regCollAccess

-- Generates programs containing the application of a t gate to a qubit
programWithTGateApp :: Gen MetaQasmProgram

programWithTGateApp = toProgWithTGateApp <$> validRegCollAccess 
  where
    toProgWithTGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTGateApp = formatToString (appGateToQubits tGateApp)

-- Generates programs containing the application of a T dagger gate to a qubit
programWithTDaggerGateApp :: Gen MetaQasmProgram

tDaggerGateApp :: RegAccessFormatter
tDaggerGateApp = singleParamGateApp (fconst "tdg") regCollAccess

programWithTDaggerGateApp = toProgWithTDaggerGateApp <$> validRegCollAccess
  where
    toProgWithTDaggerGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTDaggerGateApp  = formatToString (appGateToQubits tDaggerGateApp)

-- Takes a separator, two formatters, and generates a formatter that separates the
-- results obtained by both formatters by the separator
sepBy :: String -> MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a

sepBy separator f g = f <> toFormatter separator <%+> g

sepByComma :: MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
sepByComma = sepBy ","

twoParamGateApp :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
twoParamGateApp gateNameFormatter fstArgFormatter sndArgFormatter = gateNameFormatter <> parenthesised (fstArgFormatter `sepByComma` sndArgFormatter)

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
makeLenses ''TwoArgGateDeclInfo

twoArgGateDeclInfo :: Gen TwoArgGateDeclInfo

doesNotContainDuplicates :: Eq a => [a] -> Bool
doesNotContainDuplicates = (&&&) id nub >>> uncurry  (\\) >>> null

-- Generates information for a two qubit gate declaration such that the names of the parameters and gate are unique
twoArgGateDeclInfo = replicateM 3 freshVariable `suchThat` doesNotContainDuplicates & fmap (\[x, y, z] -> TwoArgGateDeclInfo x y z)

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
    isNotBeingOverShadowedByRegAcc  (TwoArgGateDeclInfo gateName' _ _, RegCollAccessSpec regCollName' _ _) = regCollName' /= gateName'

qubitAnnotation = later (<> ": Qbit")
bitAnnotation = later (<> ": Bit")

fstParam :: MetaQasmProgramFormatter TwoArgGateDeclInfo

fstParam = viewed fstArg string

sndParam :: MetaQasmProgramFormatter  TwoArgGateDeclInfo
sndParam = viewed sndArg string

-- Takes two formatters for the types of the arguments to the gate, a formatter for the gate body, and
-- returns a formatter that generates a two arg gate declaration with the argument types dictated by
-- the first two formatters and the body by the remaining one
gateDecl ::  MetaQasmProgramFormatter TwoArgGateDeclInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo
gateDecl fstArgFormatter sndArgFormatter gateBodyFormatter  =  (fconst "gate") <%+> (viewed gateName string) <> parenthesised (fstArgFormatter `sepByComma` sndArgFormatter) <%+> braced gateBodyFormatter

-- Generates a two qubit gate declaration that applies a cnot gate to its parameters
cnotGateDecl :: MetaQasmProgramFormatter TwoArgGateDeclInfo
cnotGateDecl = gateDecl (qubitAnnotation %. fstParam) (qubitAnnotation %. sndParam) (cnot fstParam sndParam)

-- This type represents a formatter that controls if the results of the first formatter is
-- locally scoped to the second or unscoped.
-- E.g., using ; as a scope modifier would result in the values of the first formatter being passed along
-- in an unscoped context.
type ScopeModifier a = MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a

-- Takes a formatter that modifies the scope of a gate declaration, a formatter for a gate declaration and application,
-- information about the declaration and returns a formatter that places a gate application within the scope of the
-- gate declaration
fmtGateDeclAndApp :: ScopeModifier TwoQubitGateDeclAndAppInfo -> MetaQasmProgramFormatter TwoArgGateDeclInfo -> (MetaQasmProgramFormatter a -> RegAccessFormatter) ->  TwoQubitGateDeclAndAppInfo -> MetaQasmProgram
fmtGateDeclAndApp modifier gateDeclFormatter gateAppFormatter info@(TwoArgGateDeclInfo{_gateName}, _)
  = formatToString fmtter info
  where
    fmtter = modifier (viewed _1 gateDeclFormatter) $ viewed _2 $ gateAppFormatter (toFormatter _gateName)

-- Takes a formatter for the application of a two qubit gate, the information needed for the application,
-- and generates a program with a scoped two qubit gate declaration and subsequent application
--toScopedTwoQubitGateDeclAndApp :: (MetaQasmProgramFormatter a -> RegAccessFormatter) -> TwoQubitGateDeclAndAppInfo -> MetaQasmProgram
toScopedTwoQubitGateDeclAndApp :: (RegAccessFormatter -> RegAccessFormatter) -> TwoQubitGateDeclAndAppInfo -> MetaQasmProgram
toScopedTwoQubitGateDeclAndApp gateAppFormatter = fmtGateDeclAndApp scopedDecl cnotGateDecl (appGateToQubits . gateAppFormatter)

-- Generates a MetaQasm program where a two qubit gate
-- is declared and then applied to two in-scope qubits
scopedTwoQubitGate :: Gen MetaQasmProgram
scopedTwoQubitGate =  toScopedTwoQubitGateDeclAndApp twoQubitGateApp <$> nonShadowingRegCollAccess where
    twoQubitGateApp gate = twoParamGateApp gate regCollAccess regCollAccess


-- Generates programs where a two qubit gate is applied to
-- three qubits
programWithTooManyParamsInGateApp :: Gen MetaQasmProgram

programWithTooManyParamsInGateApp = toScopedTwoQubitGateDeclAndApp  threeQubitGateApp <$> nonShadowingRegCollAccess
  where
    threeQubitGateApp gate =  gate <>  parenthesised (regCollAccess `sepByComma` regCollAccess `sepByComma` regCollAccess)

-- Generates programs where a two qubit gate is applied to
-- one qubit
programWithTooFewParamsInGateApp :: Gen MetaQasmProgram

programWithTooFewParamsInGateApp = toScopedTwoQubitGateDeclAndApp  (flip singleParamGateApp regCollAccess) <$> nonShadowingRegCollAccess

-- This type represents the information needed to create a MetaQASMProgram
-- that measures a qubit and stores the measurement in a bit
data QubitMeasurementSpec = QubitMeasurementSpec{_quantumRegCollInfo :: RegCollAccessSpec, _classicRegCollInfo :: RegCollAccessSpec}
makeLenses ''QubitMeasurementSpec

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

    toQubitMeasurement = formatToString $ scopedDecl (viewed classicRegCollInfo classicRegCollDecl) $ scopedDecl (viewed quantumRegCollInfo quantumRegCollDecl) measureQubit
    measureQubit = formatMeasurement (viewed quantumRegCollInfo regCollAccess) (viewed classicRegCollInfo regCollAccess)

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
-- are invalid due to a misplaced bit/qubit, function that generates
-- the misplaced term based on the input to the formatter, a data generator 
--  for the formatter, returns pairs of invalid programs and the misplaced term
genInvalidProgram' :: MetaQasmProgramFormatter a -> (a -> Expression) -> Gen a -> Gen InvalidProgram

genInvalidProgram' invalidProgFmtter f gen = (&&&) (formatToString invalidProgFmtter) f <$> gen

-- Given a formatter that generates MetaQASM programs that
-- are invalid due to a misplaced bit/qubit, generates pairs
-- of invalid programs and the bit/qubit responsible for
-- making the program fail
genInvalidProgram :: RegAccessFormatter -> Gen InvalidProgram

genInvalidProgram invalidProgFmtter = genInvalidProgram' invalidProgFmtter toRegAccessOnLine1 validRegCollAccess

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
    regCollApp = singleParamGateApp (viewed regCollName string)  regCollAccess

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
makeLenses ''GateThatTakesQubitAndBit

gateThatTakesQubitAndBit :: Gen GateThatTakesQubitAndBit

gateThatTakesQubitAndBit = uncurry GateThatTakesQubitAndBit <$> (twoArgGateDeclInfo >*< qubitMeasurementSpec ) `suchThat` gateDoesNotOvershadowRegColls
  where
    gateDoesNotOvershadowRegColls :: (TwoArgGateDeclInfo, QubitMeasurementSpec) -> Bool
    gateDoesNotOvershadowRegColls (declSpec, measurementInfo) = view gateName declSpec `notElem` [getQuantumRegCollName measurementInfo, getClassicalRegCollName measurementInfo]
    getQuantumRegCollName = view  $ quantumRegCollInfo . regCollName
    getClassicalRegCollName = view  $ classicRegCollInfo . regCollName


-- Generates a declaration for a gate that takes a qubit
-- and a bit and applies a hadamard gate to the qubit
scopedGateThatAppliesHadamardGateToOneArg :: Gen MetaQasmProgram

scopedGateThatAppliesHadamardGateToOneArg = formatToString scopedGate <$> gateThatTakesQubitAndBit
  where
    scopedGate = scopedDecl qregColl $ scopedDecl cregColl $ scopedDecl gate gateApp
    qregColl = viewed quantumMeasurementComponent quantumRegCollDecl
    cregColl = viewed classicalMeasurementComponent classicRegCollDecl
    gate = viewed gateInfo gateDecl'
    gateDecl' = gateDecl (qubitAnnotation %. fstParam) (bitAnnotation %. sndParam) (hadamardApp fstParam)
    gateApp = twoParamGateApp (viewed (gateInfo . gateName) string) qubit bit
    qubit = viewed quantumMeasurementComponent regCollAccess
    bit = viewed classicalMeasurementComponent regCollAccess
    quantumMeasurementComponent = measurementComponents . quantumRegCollInfo
    classicalMeasurementComponent = measurementComponents . classicRegCollInfo

sepBySemicolon :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a

sepBySemicolon  = sepBy ";"

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
-- before sequencing it with a valid command that uses it
programThatSequencesUnscopedClassicRegColl :: Gen MetaQasmProgram
programThatSequencesUnscopedClassicRegColl = formatToString (classicRegCollDecl' `sepBySemicolon`  quantumRegCollDecl' `sepBySemicolon`  formatMeasurement qubit' bit') <$>  qubitMeasurementSpec
  where
    quantumRegCollDecl' = viewed quantumRegCollInfo quantumRegCollDecl
    classicRegCollDecl' = viewed classicRegCollInfo classicRegCollDecl
    qubit' = viewed quantumRegCollInfo regCollAccess
    bit' = viewed classicRegCollInfo regCollAccess

-- Generates MetaQASM programs that are comprised of one
-- valid unrelated command sequenced with another valid command
programThatSequencesUnrelatedCommands :: Gen MetaQasmProgram
programThatSequencesUnrelatedCommands = formatToString (string % ";" %+ string)  <$> programWithValidHGateApp  <*> programWithCNotGateApp

reset :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
reset = (fconst "reset" <%+>)

-- Generates a program that resets a qubit to its default state
programThatResetsAQubit :: Gen MetaQasmProgram

programThatResetsAQubit = formatToString (quantumRegCollDecl `sepBySemicolon` reset regCollAccess) <$> validRegCollAccess

-- Generates a program that resets a bit
programThatResetsABit :: Gen InvalidProgram

programThatResetsABit = genInvalidProgram (classicRegCollDecl `sepBySemicolon` reset regCollAccess)

-- Generates a program that declares an unscoped two qubit
-- gate and applies it to two qubits
unscopedGateDeclAndApp :: Gen MetaQasmProgram

unscopedGateDeclAndApp = toUnscopedGateDeclAndApp <$>  nonShadowingRegCollAccess
  where
    toUnscopedGateDeclAndApp ::  TwoQubitGateDeclAndAppInfo -> MetaQasmProgram
    toUnscopedGateDeclAndApp = fmtGateDeclAndApp sepBySemicolon cnotGateDecl (twoParamGateApp' regCollAccess regCollAccess >>> sepBySemicolon quantumRegCollDecl)

    twoParamGateApp' :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a  -> MetaQasmProgramFormatter a
    twoParamGateApp' fstArgFormatter sndArgFormatter gateNameFormatter = twoParamGateApp gateNameFormatter fstArgFormatter sndArgFormatter


-- Generates a program that declares an unscoped two qubit
-- gate that applies a CNOT gate to its arguments
unscopedTwoQubitGateDecl :: Gen MetaQasmProgram

unscopedTwoQubitGateDecl = formatToString cnotGateDecl <$> twoArgGateDeclInfo

-- This data type represents a gate that can take any kind of parameter
data SingleParamGateInfo a = SingleParamGateInfo{_gateId :: String, _paramName :: String, _paramInfo :: a} deriving (Show)
makeLenses ''SingleParamGateInfo

-- Generates information about a gate that only takes a quantum register
-- collection
gateThatTakesARegColl :: Gen (SingleParamGateInfo RegCollAccessSpec)

gateThatTakesARegColl = ((>**<) freshVariable freshVariable validRegCollAccess) `suchThat` regCollIsNotOvershadowed & fmap (uncurry3 SingleParamGateInfo)
  where
    regCollIsNotOvershadowed :: (String, String, RegCollAccessSpec) -> Bool
    regCollIsNotOvershadowed (gateName', paramName', RegCollAccessSpec{_regCollName}) = doesNotContainDuplicates [_regCollName, gateName', paramName']

gateThatTakesARegColl' :: Gen (SingleParamGateInfo RegCollAccessSpec)
gateThatTakesARegColl' = changeParamNameToMatchRegColl <$> gateThatTakesARegColl
  where
    changeParamNameToMatchRegColl :: SingleParamGateInfo RegCollAccessSpec -> SingleParamGateInfo RegCollAccessSpec
    changeParamNameToMatchRegColl x = x & view  paramName & flip (set (paramInfo . regCollName)) x

qubitRegCollAnnotation :: RegAccessFormatter
qubitRegCollAnnotation = viewed regCollName string <> fconst ": Qbit" <> squared (viewed numOfRegs int)

singleParamGateApp' :: MetaQasmProgramFormatter (SingleParamGateInfo a) -> MetaQasmProgramFormatter (SingleParamGateInfo a)
singleParamGateApp' = singleParamGateApp fmtGateName
  where
    fmtGateName :: MetaQasmProgramFormatter (SingleParamGateInfo a)
    fmtGateName = viewed gateId string

singleParamGateDecl :: MetaQasmProgramFormatter (SingleParamGateInfo a) -> MetaQasmProgramFormatter (SingleParamGateInfo a) ->  MetaQasmProgramFormatter (SingleParamGateInfo a)
singleParamGateDecl argFormatter gateBodyFormatter = fconst "gate" <%+> (viewed gateId string) <> parenthesised argFormatter <%+> braced gateBodyFormatter

-- Takes a formatter for controlling the scope of the gate declaration relative to its application,
-- formatters for the gate declaration and application, and
-- returns a formatter that declares and applies a gate in the given scope
singleParamGateDeclAndApp :: ScopeModifier (SingleParamGateInfo a) -> MetaQasmProgramFormatter (SingleParamGateInfo a) -> MetaQasmProgramFormatter (SingleParamGateInfo a)  -> MetaQasmProgramFormatter (SingleParamGateInfo a) -> MetaQasmProgramFormatter (SingleParamGateInfo a)

singleParamGateDeclAndApp scopedTo gateArgFmtter gateBodyFmtter gateAppFmtter = singleParamGateDecl gateArgFmtter gateBodyFmtter `scopedTo` singleParamGateApp' gateAppFmtter
multilineSingleParamGateDeclAndApp = singleParamGateDeclAndApp (sepBy "\n;")
oneLineSingleParamGateDeclAndApp = singleParamGateDeclAndApp sepBySemicolon

-- Generates a program that contains the application of an
-- unscoped gate that takes a quantum register collection to
-- a quantum register collection
multilineUnscopedGateWithQuantumRegCollParam :: Gen MetaQasmProgram
multilineUnscopedGateWithQuantumRegCollParam = formatToString multilineDecl <$> gateThatTakesARegColl'
  where
    multilineDecl :: MetaQasmProgramFormatter (SingleParamGateInfo RegCollAccessSpec)
    multilineDecl  =
      viewed paramInfo quantumRegCollDecl
      `sepBySemicolonOnNewLine`
      multilineSingleParamGateDeclAndApp gateParam gateBody gateApp

    gateParam = viewed paramInfo qubitRegCollAnnotation
    gateBody = viewed paramInfo  $ hadamardApp regCollAccess
    gateApp = viewed (paramInfo . regCollName) string
    sepBySemicolonOnNewLine = sepBy "\n;"


sepByColon :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
sepByColon = sepBy ":"

-- Generates a invalid program consisting of a single parameter gate
-- declaration where the parameter is an empty register collection
unscopedGateThatTakesAnEmptyRegColl :: Gen MetaQasmProgram

unscopedGateThatTakesAnEmptyRegColl = formatToString invalidGateDecl <$> gateThatTakesARegColl'
  where
    invalidGateDecl :: MetaQasmProgramFormatter (SingleParamGateInfo RegCollAccessSpec)
    invalidGateDecl = singleParamGateDecl emptyQuantRegColl $ fconst "h(x)"
    emptyQuantRegColl = viewed paramName string `sepByColon` fconst "Qbit[0]"


-- Generates pairs of invalid programs that apply a unitary operation
-- to an element of a classical register collection and the aforementioned
-- element
gateThatAppliesUnitaryToClassicalRegCollElem :: Gen InvalidProgram
gateThatAppliesUnitaryToClassicalRegCollElem = genInvalidProgram' invalidGateDecl genSelectedBit gateThatTakesARegColl'
  where
    invalidGateDecl = singleParamGateDecl (viewed paramInfo classicalRegCollAnnotation') $ viewed paramInfo hadamardApp'
    genSelectedBit = view paramInfo >>> toRegAccessOnLine1
    classicalRegCollAnnotation' :: RegAccessFormatter
    classicalRegCollAnnotation' = viewed regCollName string `sepByColon` fconst "Bit" <> squared (viewed numOfRegs int)

-- Generates a valid higher ordered gate which is then
-- applied to a single qubit unitary
higherOrderedGateDeclAndApp :: Gen MetaQasmProgram
higherOrderedGateDeclAndApp =  formatToString gateDeclAndApp <$> gateThatTakesARegColl
  where
    higherOrderedUnitaryDecl :: MetaQasmProgramFormatter (SingleParamGateInfo RegCollAccessSpec)
    higherOrderedUnitaryDecl = singleParamGateDecl (gateArg <> fconst ": Circuit(Qbit)") $ singleParamGateApp gateArg $ viewed paramInfo regCollAccess
    gateDeclAndApp = viewed paramInfo quantumRegCollDecl
      `sepBySemicolon`
      higherOrderedUnitaryDecl
      `sepBySemicolon`
      singleParamGateApp' hGate
    hGate = fconst "h"
    gateArg = viewed paramName string


-- This type represents the information in a guard
-- for a conditional gate execution
data GateGuard = GateGuard{_expectedValue :: Int, _bitBeingTested :: RegCollAccessSpec}
makeLenses ''GateGuard

validGuard :: Gen GateGuard
validGuard = (>*<) arbitrarySizedNatural validRegCollAccess & fmap (uncurry GateGuard)

-- This data type represents the information needed to construct a MetaQASM program
-- that conditionally executes a gate
data ConditionalGateInfo = ConditionalGateInfo{_guardInfo :: GateGuard, _gateData :: RegCollAccessSpec}
makeLenses ''ConditionalGateInfo

conditionalGateInfo :: Gen ConditionalGateInfo

conditionalGateInfo = (>*<) validGuard validRegCollAccess `suchThat` isGateNotOvershadowingGuard & fmap (uncurry ConditionalGateInfo)
  where
    isGateNotOvershadowingGuard :: (GateGuard, RegCollAccessSpec) -> Bool
    isGateNotOvershadowingGuard  = liftA2 (/=) (view (_1 . bitBeingTested . regCollName)) $ view (_2 . regCollName)

-- Generates a program that conditionally executes
-- a gate depending on the value of the guard
conditionalGateExecution :: Gen MetaQasmProgram
conditionalGateExecution = formatToString potentialGateExec <$> conditionalGateInfo
  where
    potentialGateExec :: MetaQasmProgramFormatter ConditionalGateInfo
    potentialGateExec =
      viewed testedBit classicRegCollDecl
      `sepBySemicolon`
      viewed gateData quantumRegCollDecl
      `sepBySemicolon`
      execGateIf
      expectedBitVal
      (viewed testedBit regCollAccess)
      (viewed gateData hadamardApp')

    testedBit = guardInfo . bitBeingTested
    expectedBitVal = viewed (guardInfo . expectedValue) int
    execGateIf :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
    execGateIf expectedBitVal' actualBitVal gate = fconst "if" <%+> parenthesised (actualBitVal `eq` expectedBitVal') <%+> braced gate
    eq = sepBy "=="

incRegCount = over (paramInfo . numOfRegs) (+ 1)

-- Generates a MetaQASM program that applies a gate taking a
-- register collection of size N to a register collection of size N + 1
programWithGateAppToSubtypeOfExpectedRegColl ::  Gen MetaQasmProgram
programWithGateAppToSubtypeOfExpectedRegColl = formatToString gateApp <$> gateThatTakesARegColl'
  where
    gateApp =
      mapf incRegCount  (viewed paramInfo quantumRegCollDecl)
      `sepBySemicolon`
      oneLineSingleParamGateDeclAndApp gateArg gateBody gateAppTo

    gateArg = viewed paramInfo qubitRegCollAnnotation
    gateBody = viewed paramInfo hadamardApp'
    gateAppTo = viewed paramName string
    incRegCount = over (paramInfo . numOfRegs) (+ 1)

-- Generates a valid program that applies a sequence of gates
-- to a register collection
programThatSequencesGates :: Gen MetaQasmProgram
programThatSequencesGates = formatToString gateSequenceApp <$> gateThatTakesARegColl'
  where
    gateSequenceApp =
      viewed paramInfo quantumRegCollDecl
      `sepBySemicolon`
      oneLineSingleParamGateDeclAndApp param gateBody (viewed (paramInfo . regCollName) string)

    gateBody = viewed paramInfo hadamardApp' `sepBySemicolon` viewed paramInfo tDaggerGateApp
    param = viewed paramInfo qubitRegCollAnnotation

type GateThatTakesARegColl = SingleParamGateInfo RegCollAccessSpec

type HigherOrderedGate = SingleParamGateInfo GateThatTakesARegColl

summing :: Fold s a -> Fold s a  -> Fold s a
summing f g = folding $ \s -> s ^.. f ++ s ^.. g

-- Takes the information describing a gate f that takes
-- another gate g and for both gates returns its name
-- and the name of the given parameter
gateAndParamNames :: Fold HigherOrderedGate Identifier
gateAndParamNames =  extractGateAndParamName `summing` (paramInfo . extractGateAndRegCollName)
  where

    extractGateAndParamName :: Fold (SingleParamGateInfo a) Identifier
    extractGateAndParamName =  gateId `summing` paramName
    extractGateAndRegCollName :: Fold GateThatTakesARegColl Identifier
    extractGateAndRegCollName = extractGateAndParamName `summing` (paramInfo . regCollName)

higherOrderedGateInfo :: Gen HigherOrderedGate
higherOrderedGateInfo = (SingleParamGateInfo <$> freshVariable <*> freshVariable <*> gateThatTakesANonSingletonRegColl )`suchThat` gateDeclsAreNotBeingOvershadowed
  where
    gateDeclsAreNotBeingOvershadowed :: HigherOrderedGate -> Bool
    gateDeclsAreNotBeingOvershadowed  = toListOf gateAndParamNames >>> doesNotContainDuplicates
    doesNotContainDuplicates :: [Identifier] -> Bool
    doesNotContainDuplicates = (&&&) id nub >>> uncurry  (\\) >>> null
    gateThatTakesANonSingletonRegColl = incRegCount <$> gateThatTakesARegColl

circuitAnnotation :: MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a -> MetaQasmProgramFormatter a
circuitAnnotation name circuitTypes  = name <> fconst ":" <%+> fconst "Circuit" <> parenthesised circuitTypes

-- Generates a MetaQASM program that applies a gate
-- expecting a circuit of type K to a circuit of type
-- K', where K' is a subtype of K
programThatAppliesGateToCircSubType :: Gen MetaQasmProgram
programThatAppliesGateToCircSubType = formatToString gateApp <$>  higherOrderedGateInfo
  where
    gateApp :: MetaQasmProgramFormatter HigherOrderedGate
    gateApp =
      viewed (paramInfo . paramInfo) quantumRegCollDecl
      `sepBySemicolon`
      singleParamGateDecl gateArg body
      `sepBySemicolon`
      mapf (view paramInfo >>> decRegCount) (singleParamGateDecl (viewed paramInfo qubitRegCollAnnotation) (viewed paramInfo tDaggerGateApp))
      `sepBySemicolon`
      singleParamGateApp (viewed gateId string) (viewed (paramInfo . gateId) string)

    gateArg = circuitAnnotation (viewed paramName string) (viewed (paramInfo . paramInfo) nSizedQuantColl)
    body = singleParamGateApp (viewed paramName string) $ viewed (paramInfo . paramInfo . regCollName) string
    nSizedQuantColl :: MetaQasmProgramFormatter RegCollAccessSpec
    nSizedQuantColl = fconst "Qbit" <> squared (viewed numOfRegs int)
    incRegCount = over (paramInfo . paramInfo . numOfRegs) (+1)
    decRegCount = over (paramInfo . numOfRegs) (subtract 1)
