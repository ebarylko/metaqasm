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

-- Takes a name for a variable, the line it was found, and constructs
-- a MetaQASM term representing the variable.
genVar :: Identifier -> LineNumber -> Expression

genVar varName lineNum =  Var $ WithContext varName lineNum

type GateFn = Expression -> GateApp

toExpr = fromJust . Vary.into @Expression

toCommand = fromJust . Vary.into @Command

-- Takes text representing a MetaQASM command, the expected command, and
-- checks that the expected command is obtained after parsing the text 
shouldParseToCommand text expected = (fmap toCommand . parseText) text `shouldBe` Right expected

-- Takes a gate, an expression, and returns a command that
-- consists solely of the gate applied to the expression
toGateWithinCommand :: GateFn -> Expression -> Command
toGateWithinCommand gate = Gate . (gate $)


-- Takes text representing a MetaQASM command, the expected command, and
-- checks that the expected command is obtained after parsing the text
shouldParseToExpr text expected = (fmap toExpr . parseText) text `shouldBe` Right expected

spec :: Spec

spec = do
  describe "Parsing MetaQASM programs" $ do
    describe "Parsing variables" $ do
      it "Generates a variable with the context of where it was found" $ do
        "varName" `shouldParseToExpr` genVar "varName" (LineNumber 1)
    describe "Parsing gate applications" $
      it "Generates a term representing the application" $ do
        "tdg(varName)" `shouldParseToCommand` toGateWithinCommand Tdg (genVar "varName" (LineNumber 1))
        "h(varName)" `shouldParseToCommand` toGateWithinCommand H (genVar "varName" (LineNumber 1))
        "t(varName)" `shouldParseToCommand` toGateWithinCommand T (genVar "varName" (LineNumber 1))

