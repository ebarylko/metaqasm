{-# LANGUAGE OverloadedStrings #-}
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
                 programWithTooManyParamsInGateApp)
  where

import Test.QuickCheck
import Formatting
import Syntax(Identifier, NonNeg(..))
import Control.Arrow((&&&), (>>>))
import Test.QuickCheck.Instances.Tuple ((>**<), (>*<))
import Data.Function((&))
import Typecheck(TypeEvaluationError(..))
import Control.Monad(replicateM)
import Data.List(nub)
import Data.Text.Lazy.Builder(fromString)

builtInGates :: [String]
builtInGates = ["h", "cx", "t", "tdg"]

outOfScopeRegColl :: Gen String
outOfScopeRegColl = ((:) <$> lowerCaseLetter <*> listOf alphaNumeric) `suchThat` (not . (`elem` builtInGates))
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

-- Takes a name for a register collection, the number of registers in
-- the collection, and generates a string of the form 'creg collName[numOfRegisters]'
quantumRegCollDecl :: RegAccessFormatter
quantumRegCollDecl = "creg "  % (accessed _regCollName string) <> squared (accessed _numOfRegs int)

-- Takes a formatter for a register access specification and generates a formatter
-- for applying a gate to the accessed qubit/s
appGateToQubits :: RegAccessFormatter -> RegAccessFormatter
appGateToQubits gate = quantumRegCollDecl % " in " <>  braced gate

toProgWithGateApp :: RegAccessFormatter  -> RegCollAccessSpec -> MetaQasmProgram
toProgWithGateApp = formatToString

toFormatter = now . fromString

singleQubitGateApp' :: String -> RegAccessFormatter
singleQubitGateApp' gate = toFormatter gate % parenthesised regCollAccess

hadamardApp :: RegAccessFormatter
hadamardApp = singleQubitGateApp' "h"

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
    toProgWithEmptyRegCollDecl = toProgWithGateApp (emptyRegCollDecl % " in " <>  braced hadamardApp) 
    emptyRegCollDecl = "creg" %+ (accessed _regCollName string) % "[0]"

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
tGateApp = singleQubitGateApp' "t"

-- Generates programs containing the application of a t gate to a qubit
programWithTGateApp :: Gen MetaQasmProgram

programWithTGateApp = toProgWithTGateApp <$> genValidRegCollAccessSpec 
  where
    toProgWithTGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTGateApp = toProgWithGateApp (appGateToQubits tGateApp)

-- Generates programs containing the application of a T dagger gate to a qubit
programWithTDaggerGateApp :: Gen MetaQasmProgram

tDaggerGateApp :: RegAccessFormatter
tDaggerGateApp = singleQubitGateApp' "tdg"

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
    isNotBeingOverShadowedByRegAcc  (TwoQubitGateDeclInfo gateName fstQubitName sndQubitName, RegCollAccessSpec regCollName _ _) = not $ regCollName `elem` [gateName, fstQubitName, sndQubitName]


-- Generates a two qubit gate declaration that applies a cnot gate to its parameters
toGateDecl :: Format MetaQasmProgram (TwoQubitGateDeclInfo -> MetaQasmProgram)
toGateDecl = "gate " % gateName <> parenthesised gateArgs <> " " % braced ("cx" % parenthesised cnotArgs)
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
fmtGateDeclAndApp gateDeclFormatter gateAppFormatter = fmtGateDecl % " in " <> fmtGateApp
  where
    fmtGateDecl = accessed fst gateDeclFormatter
    fmtGateApp = braced $ accessed snd $ appGateToQubits gateAppFormatter

-- Generates a MetaQasm program where a two qubit gate
-- is declared and then applied to two in-scope qubits
programWithTwoQubitGateDeclAndApp :: Gen MetaQasmProgram

-- Takes a formatter for a gate declaration, a formatter for a gate application, the information needed for both formatters, and generates a
-- MetaQASM program based on both formatters and the info that declares an n-ary gate and applies it later on
fmtGateDeclAndApp' :: Format MetaQasmProgram (TwoQubitGateDeclInfo -> MetaQasmProgram) -> (String -> RegAccessFormatter) ->  TwoQubitGateDeclAndAppInfo -> MetaQasmProgram

fmtGateDeclAndApp' gateDeclFormatter gateAppFormatter info@(TwoQubitGateDeclInfo gateName _ _, _) = formatToString (fmtGateDeclAndApp gateDeclFormatter (gateAppFormatter gateName)) info

programWithTwoQubitGateDeclAndApp =  fmtGateDeclAndApp' toGateDecl twoQubitGateApp <$> nonShadowingRegCollAccess
  where
    twoQubitGateApp gate = toFormatter gate %  parenthesised (regCollAccess % ", " <> regCollAccess)

-- Generates programs where a two qubit gate is applied to
-- three qubits
programWithTooManyParamsInGateApp :: Gen MetaQasmProgram

programWithTooManyParamsInGateApp = fmtGateDeclAndApp' toGateDecl threeQubitGateApp <$> nonShadowingRegCollAccess
  where
    threeQubitGateApp gate = toFormatter gate %  parenthesised (regCollAccess % ", " <> regCollAccess % ", " <> regCollAccess)

-- Generates programs where a two qubit gate is applied to
-- one qubit
programWithTooFewParamsInGateApp :: Gen MetaQasmProgram

programWithTooFewParamsInGateApp = fmtGateDeclAndApp' toGateDecl singleQubitGateApp' <$> nonShadowingRegCollAccess
