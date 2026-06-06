module Generators(outOfScopeRegColl,
                  outOfScopeExpr,
                  Expr)
  where

import Test.QuickCheck

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
