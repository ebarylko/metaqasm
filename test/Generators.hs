{-# LANGUAGE OverloadedStrings #-}
module Generators(outOfScopeRegColl,
                  outOfScopeExpr,
                  programWithQubitInScope,
                  Expr,
                  programWithEmptyRegCollDecl)
  where

import Test.QuickCheck
import Formatting
import Syntax(Identifier, Command (numOfRegs))
import Control.Arrow((&&&), (>>>))
import Test.QuickCheck.Instances.Tuple ((>**<))
import Data.Function((&))


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

type Expr = String

outOfScopeVarName = outOfScopeRegColl

outOfScopeRegAccess :: Gen Expr
outOfScopeRegAccess = (++) <$> outOfScopeRegColl <*> pure "[0]"

outOfScopeExpr :: Gen Expr
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

-- Takes a specification detailing a valid access
-- of a register collection x with n registers and
-- generates a declaration of a quantum register collection
-- with name x and n registers
genQuantumRegDecl :: RegCollAccessSpec -> Expr
genQuantumRegDecl (RegCollAccessSpec regCollId numOfRegs' _) = formatToString ("creg" %+ string % squared int ) regCollId  numOfRegs'

regCollAccess = string % squared int

-- Takes a specification detailing a valid access
-- of the ith element of a register collection and
-- generates the string representation of such an
-- access
genRegCollAccess :: RegCollAccessSpec -> Expr
genRegCollAccess (RegCollAccessSpec regCollId _ wantedRegIdx') = formatToString regCollAccess regCollId wantedRegIdx'

-- Generates metaQASM code where a hadamard gate is applied to
-- a qubit that is in scope
programWithQubitInScope :: Gen Expr

programWithQubitInScope =  genValidRegCollAccessSpec  & convertToMetaQasmProgram
  where
    formatInScopeRegAccess :: Expr -> Expr -> Expr
    formatInScopeRegAccess = formatToString (string %+ "in" %+ braced  ("h" % parenthesised string)  )

    convertToMetaQasmProgram :: Gen RegCollAccessSpec -> Gen Expr
    convertToMetaQasmProgram = fmap ((&&&) genQuantumRegDecl genRegCollAccess >>> uncurry formatInScopeRegAccess)

-- Generates metaQASM code where an empty
-- register collection is declared
programWithEmptyRegCollDecl :: Gen Expr

programWithEmptyRegCollDecl =  toProgWithEmptyRegCollDecl <$> outOfScopeRegColl <*> arbitrarySizedNatural
  where
    toProgWithEmptyRegCollDecl regCollName regIdx = formatToString (emptyRegCollDecl %+ "in" %+  braced hadamardApp) regCollName regCollName regIdx
    emptyRegCollDecl = "creg" %+ string % "[0]"
    hadamardApp = "h" % parenthesised regCollAccess
