{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Generators(outOfScopeRegColl,
                  outOfScopeExpr,
                  programWithQubitInScope,
                  MetaQasmProgram,
                  programWithEmptyRegCollDecl,
                 programWithInvalidRegAccess,
                 ProgramWithExpectedErr,
                 programWithTGateApp,
                 programWithTDaggerGateApp,
                 programWithCNotGateApp
                 )
  where

import Test.QuickCheck
import Formatting
import Syntax(Identifier, NonNeg(..))
import Control.Arrow((&&&))
import Test.QuickCheck.Instances.Tuple ((>**<))
import Data.Function((&))
import Typecheck(TypeEvaluationError(..))
import Control.Lens (makeLenses)


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

makeLenses ''RegCollAccessSpec

-- Formats the access of a register in a register collection,
-- generating a string of the form 'regName[regIdx]'
regCollAccess :: RegAccessFormatter
regCollAccess = (viewed regCollName string) <> squared (viewed wantedRegIdx int)

-- Takes a name for a register collection, the number of registers in
-- the collection, and generates a string of the form 'creg collName[numOfRegisters]'
quantumRegCollDecl :: RegAccessFormatter
quantumRegCollDecl = "creg "  % (viewed regCollName string) <> squared (viewed numOfRegs int)

-- Takes a formatter for a register access specification and generates a formatter
-- for applying a gate to the accessed qubit/s
appGateToInScopeQubits :: RegAccessFormatter -> RegAccessFormatter
appGateToInScopeQubits gate = quantumRegCollDecl % " in " <>  braced gate

toProgWithGateApp :: RegAccessFormatter  -> RegCollAccessSpec -> MetaQasmProgram
toProgWithGateApp = formatToString

singleQubitGateApp' gate = gate % parenthesised regCollAccess

hadamardApp = singleQubitGateApp' "h"

-- Generates metaQASM code where a hadamard gate is applied to
-- a qubit that is in scope
programWithQubitInScope :: Gen MetaQasmProgram

programWithQubitInScope =  toProgWithHGateApp <$> genValidRegCollAccessSpec
  where
    toProgWithHGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithHGateApp =  toProgWithGateApp (appGateToInScopeQubits hadamardApp)

-- Generates metaQASM code where an empty
-- register collection is declared
programWithEmptyRegCollDecl :: Gen MetaQasmProgram

programWithEmptyRegCollDecl =  toProgWithEmptyRegCollDecl <$> genInvalidRegCollAccessSpec
  where
    toProgWithEmptyRegCollDecl = toProgWithGateApp (emptyRegCollDecl % " in " <>  braced hadamardApp) 
    emptyRegCollDecl = "creg" %+ (viewed regCollName string) % "[0]"

-- Represents pairs of programs and the errors obtained when
-- running them
type ProgramWithExpectedErr = (MetaQasmProgram, TypeEvaluationError)

-- Generate a pair of programs that access invalid registers
-- and the expected register access error received when running them
programWithInvalidRegAccess :: Gen ProgramWithExpectedErr

programWithInvalidRegAccess = genInvalidRegCollAccessSpec & fmap ((&&&) toProgWithInvalidAccess toErr)
  where
    toProgWithInvalidAccess :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithInvalidAccess = toProgWithGateApp (appGateToInScopeQubits hadamardApp)

    toErr :: RegCollAccessSpec -> TypeEvaluationError
    toErr (RegCollAccessSpec regCollId _ regIdx') = InvalidRegAccess regCollId (NonNeg regIdx')

tGateApp = singleQubitGateApp' "t"

-- Generates programs containing the application of a t gate to a qubit
programWithTGateApp :: Gen MetaQasmProgram

programWithTGateApp = toProgWithTGateApp <$> genValidRegCollAccessSpec 
  where
    toProgWithTGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTGateApp = toProgWithGateApp (appGateToInScopeQubits tGateApp)

-- Generates programs containing the application of a T dagger gate to a qubit
programWithTDaggerGateApp :: Gen MetaQasmProgram

tDaggerGateApp = singleQubitGateApp' "tdg"

programWithTDaggerGateApp = toProgWithTDaggerGateApp <$> genValidRegCollAccessSpec 
  where
    toProgWithTDaggerGateApp :: RegCollAccessSpec -> MetaQasmProgram
    toProgWithTDaggerGateApp  = toProgWithGateApp (appGateToInScopeQubits tDaggerGateApp)

programWithCNotGateApp  = formatToString toCnotGateApp <$> genValidRegCollAccessSpec
  where
    toCnotGateApp :: RegAccessFormatter
    toCnotGateApp = appGateToInScopeQubits cNotGateApp
    cNotGateApp :: RegAccessFormatter
    cNotGateApp = "cx" % parenthesised (regCollAccess % ", " <> regCollAccess)

