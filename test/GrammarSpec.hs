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


toExpr :: Term -> Expression
toExpr = fromJust . Vary.into @Expression

toCommand :: Term -> Command
toCommand = fromJust . Vary.into @Command

-- Takes a MetaQASM program representing a command, the command expected
-- after parsing the program, and checks that the expected command is obtained after parsing the text
shouldParseToCommand :: MetaQasmProgram -> Command -> Expectation
shouldParseToCommand text expected = (fmap toCommand . parseText) text `shouldBe` Right expected


-- Takes the name of a gate, the line it was applied on,the parameters of the gate,
-- and returns a command that consists solely of the gate applied to the parameters
toGateWithinCommand :: String -> LineNumber -> [Expression] -> Command
toGateWithinCommand gateName line = Gate . App (WithContext gateName line)

line1 :: LineNumber
line1 = LineNumber 1

onLine1 :: a -> WithContext a LineNumber
onLine1 = flip WithContext line1

regAccess :: Identifier -> Int -> Expression
regAccess regCollname idx = RegisterAccess (onLine1 regCollname) (onLine1 (NonNeg idx))

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
        "tdg(varName)" `shouldParseToCommand` (toGateWithinCommand "tdg" (LineNumber 1)) [genVar "varName" (LineNumber 1)]
        "h(varName)" `shouldParseToCommand` (toGateWithinCommand "h" (LineNumber 1)) [genVar "varName" (LineNumber 1)]
        "t(varName)" `shouldParseToCommand` (toGateWithinCommand "t" (LineNumber 1)) [genVar "varName" (LineNumber 1)]
        "cx(var1, var2)" `shouldParseToCommand` (toGateWithinCommand "cx" (LineNumber 1)) [(genVar "var1" (LineNumber 1)), (genVar "var2" (LineNumber 1))]

    describe "Parsing gate declarations" $
      it "Generates a term representing the declaration and its application" $ do
        let expectedGateArgs = [GateArg "x" Qbit, GateArg "y" Qbit]
        let cnot = onLine1 "cx"
        let expectedGateBody = App cnot [genVar "x" (LineNumber 1),  genVar "y" (LineNumber 1)]
        let fnName = onLine1 "f" 
        let expectedGateApp = Gate (App fnName [regAccess "c" 0,
                                                regAccess "c" 1])
        let twoRegs = onLine1 (NonNeg 2)
        let expectedInnerExpr = QRegDeclIn "c" twoRegs expectedGateApp
        "gate f(x: Qbit, y: Qbit) {cx(x, y)} in {creg c[2] in {f(c[0], c[1])}}" `shouldParseToCommand` GateDecl "f" expectedGateArgs expectedGateBody expectedInnerExpr
