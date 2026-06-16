{-# LANGUAGE TypeApplications #-}
module GrammarSpec(spec) where

import Test.Hspec
import Grammar(parseText)
import Syntax(Expression(..),
           WithContext(..),
           Identifier,
           GateApp(..),
           NonNeg(..),
           Command(..),
           GateArg(..),
           TermType(..))
import Lexer(LineNumber(..))
import qualified Vary
import Data.Maybe(fromJust)
import Generators (MetaQasmProgram)
import Typecheck(Term)

-- Takes a name for a variable, the line it was found, and constructs
-- a MetaQASM term representing the variable.
genVar :: Identifier -> LineNumber -> Expression

genVar varName lineNum =  Var $ WithContext varName lineNum

type GateFn = Expression -> GateApp

toExpr :: Term -> Expression
toExpr = fromJust . Vary.into @Expression

toCommand :: Term -> Command
toCommand = fromJust . Vary.into @Command

-- Takes a MetaQASM program representing a command, the command expected
-- after parsing the program, and checks that the expected command is obtained after parsing the text
shouldParseToCommand :: MetaQasmProgram -> Command -> Expectation
shouldParseToCommand text expected = (fmap toCommand . parseText) text `shouldBe` Right expected

-- Takes a gate, an expression, and returns a command that
-- consists solely of the gate applied to the expression
toGateWithinCommand :: GateFn -> Expression -> Command
toGateWithinCommand gate = Gate . (gate $)


-- Takes a program representing a MetaQASM expression, the expression that should
-- be obtained after parsing the program, and checks that the expected expression
-- is equivalent to the parsed program
shouldParseToExpr :: MetaQasmProgram -> Expression -> Expectation
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
        "cx(var1, var2)" `shouldParseToCommand` toGateWithinCommand (ControlledNot (genVar "var1" (LineNumber 1))) (genVar "var2" (LineNumber 1))

    describe "Parsing gate declarations" $
      it "Generates a term representing the declaration and its application" $ do
        let expectedGateArgs = [GateArg "x" Qbit, GateArg "x" Qbit]
        let expectedGateBody = ControlledNot ( genVar "x" (LineNumber 1)) ( genVar "y" (LineNumber 1))
        let expectedGateApp = Gate (App "f" [RegisterAccess (WithContext "c" (LineNumber 1)) (WithContext (NonNeg 0) (LineNumber 1)),
                                             RegisterAccess (WithContext "c" (LineNumber 1)) (WithContext (NonNeg 1) (LineNumber 1))] )
        let expectedInnerExpr = QRegDeclIn "c" (WithContext (NonNeg 1) (LineNumber 1)) expectedGateApp
        "gate f(x: Qbit, y: Qbit) {cx(x, y)} in {creg c[2] in {f(c[0], c[1])}}" `shouldParseToCommand` GateDecl "f" expectedGateArgs expectedGateBody expectedInnerExpr
