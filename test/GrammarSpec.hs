{-# LANGUAGE TypeApplications #-}
module GrammarSpec(spec) where

import Test.Hspec
import Grammar(parseText)
import Syntax(Expression(..),
           WithContext(..),
           Identifier,
           GateApp(..))
import Lexer(LineNumber(..))
import Typecheck(Term)
import qualified Vary
import Data.Maybe(fromJust)

-- Takes a name for a variable, the line it was found, and constructs
-- a MetaQASM term representing the variable.
genVar :: Identifier -> LineNumber -> Expression

genVar varName lineNum =  Var $ WithContext varName lineNum

type Gate = Expression -> GateApp

-- Takes an expression, the gate to apply to it, and  and generates the
-- MetaQASM term corresponding to the application of the gate to the
-- expression
genGateApp :: Gate -> Expression -> GateApp

genGateApp = ($)

toExpr = fromJust . Vary.into @Expression

spec = do
  describe "Parsing MetaQASM programs" $ do
    describe "Parsing variables" $ do
      it "Generates a variable with the context of where it was found" $ do
        (fmap toExpr . parseText) "varName" `shouldBe` (Right .  genVar "varName") (LineNumber 1)
--    describe "Parsing T dagger applications" $ do
--      it "Generates a term representing the application" $ do
--        parseText "tdg(varName)" `shouldBe` (Right  . genGateApp Tdg) (genVar "varName" (LineNumber 1))
