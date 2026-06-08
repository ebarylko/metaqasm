{-# LANGUAGE OverloadedStrings #-}
module Generators(outOfScopeRegColl,
                  outOfScopeExpr,
                  programWithQubitInScope,
                  Expr)
  where

import Test.QuickCheck
import Formatting
import Syntax(Identifier)
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


-- This data type represents a valid request to access the ith element of a register collection
-- with n >= i registers
data ValidRegCollAccess = ValidRegCollAccess{_regCollName :: Identifier, _numOfRegs :: Int, _wantedRegIdx :: Int}

genValidRegCollAccess :: Gen ValidRegCollAccess

genValidRegCollAccess = (>**<) outOfScopeRegColl posNum arbitrarySizedNatural  `suchThat` isAccessingValidReg & fmap (uncurry3 ValidRegCollAccess)
  where
    isAccessingValidReg (_, numOfElems, requestedRegIdx) = numOfElems > requestedRegIdx
    posNum :: Gen Int
    posNum = getPositive <$> arbitrary

    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f = \(x, y, z) -> f x y z


-- Takes a specification detailing a valid access
-- of a register collection x with n registers and
-- generates a declaration of a register with name x and
-- n registers
genQuantumRegDecl :: ValidRegCollAccess -> Expr
genQuantumRegDecl (ValidRegCollAccess regCollId numOfRegs' _) = formatToString ("creg" %+ string % squared int ) regCollId  numOfRegs'

-- Takes a specification detailing a valid access
-- of the ith element of a register collection and
-- generates the string representation of such an
-- access
genRegCollAccess :: ValidRegCollAccess -> Expr
genRegCollAccess (ValidRegCollAccess regCollId _ wantedRegIdx') = formatToString (string % squared int) regCollId wantedRegIdx'

-- Generates metaQASM code where a hadamard gate is applied to
-- a qubit that is in scope
programWithQubitInScope :: Gen Expr
programWithQubitInScope =  genValidRegCollAccess  & fmap ((&&&) genQuantumRegDecl genRegCollAccess >>> uncurry formatInScopeRegAccess)
  where
    formatInScopeRegAccess :: Expr -> Expr -> Expr
    formatInScopeRegAccess = formatToString (string %+ "in" % braced  ("h" % parenthesised string)  )
