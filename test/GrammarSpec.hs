module GrammarSpec(spec) where

import Test.Hspec
import Grammar(parseText)
import Syntax(Expression(..),
           WithContext(..),
           Identifier)
import Lexer(LineNumber(..))
import Typecheck(Term)
import qualified Vary

-- Takes a name for a variable, the line it was found, and constructs
-- a MetaQASM term representing the variable.
genVar :: Identifier -> LineNumber -> Term

genVar varName lineNum = Vary.from $ Var $ WithContext varName lineNum

spec = do
  describe "Parsing MetaQASM terms" $ do
    describe "Parsing variables" $ do
      it "Generates a variable with the context of where it was found" $ do
        parseText "varName" `shouldBe` Right (genVar "varName" (LineNumber 1))
