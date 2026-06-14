{-# LANGUAGE TypeApplications #-}
module GrammarSpec(spec) where

import Test.Hspec
import Grammar(parseText)
import Syntax(Expression(..),
           WithContext(..),
           Identifier,
           GateApp(..),
           Command(..))
import Lexer(LineNumber(..))
import Typecheck(Term)
import qualified Vary
import Data.Maybe(fromJust)
import Text.Printf (PrintfArg(parseFormat))

-- Takes a name for a variable, the line it was found, and constructs
-- a MetaQASM term representing the variable.
genVar :: Identifier -> LineNumber -> Expression

genVar varName lineNum =  Var $ WithContext varName lineNum

type GateFn = Expression -> GateApp

-- Takes an expression, the gate to apply to it, and  and generates the
-- MetaQASM term corresponding to the application of the gate to the
-- expression
genGateApp :: GateFn -> Expression -> GateApp

genGateApp = ($)

toExpr = fromJust . Vary.into @Expression

toCommand = fromJust . Vary.into @Command

shouldParseToCommand text expected = (fmap toCommand . parseText) text `shouldBe` expected

spec :: Spec

spec = do
  describe "Parsing MetaQASM programs" $ do
    describe "Parsing variables" $ do
      it "Generates a variable with the context of where it was found" $ do
        (fmap toExpr . parseText) "varName" `shouldBe` (Right .  genVar "varName") (LineNumber 1)
    describe "Parsing gate applications" $
      it "Generates a term representing the application" $ do
        "tdg(varName)" `shouldParseToCommand` (Right  . Gate . genGateApp Tdg) (genVar "varName" (LineNumber 1))
        "h(varName)" `shouldParseToCommand` (Right  . Gate . genGateApp H) (genVar "varName" (LineNumber 1))
        "t(varName)" `shouldParseToCommand` (Right  . Gate . genGateApp T) (genVar "varName" (LineNumber 1))

