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

-- Takes text representing a MetaQASM command, the expected command, and
-- checks that the expected command is obtained after parsing the text 
shouldParseToCommand text expected = (fmap toCommand . parseText) text `shouldBe` expected

-- Takes a gate, an expression, and returns a command that
-- consists solely of the gate applied to the expression
toGateWithinCommand :: GateFn -> Expression -> Command

toGateWithinCommand gate = Gate . (gate $)


spec :: Spec

spec = do
  describe "Parsing MetaQASM programs" $ do
    describe "Parsing variables" $ do
      it "Generates a variable with the context of where it was found" $ do
        (fmap toExpr . parseText) "varName" `shouldBe` (Right .  genVar "varName") (LineNumber 1)
    describe "Parsing gate applications" $
      it "Generates a term representing the application" $ do
        "tdg(varName)" `shouldParseToCommand` (Right  . toGateWithinCommand Tdg) (genVar "varName" (LineNumber 1))
        "h(varName)" `shouldParseToCommand` (Right  . toGateWithinCommand H) (genVar "varName" (LineNumber 1))
        "t(varName)" `shouldParseToCommand` (Right  . toGateWithinCommand T) (genVar "varName" (LineNumber 1))

