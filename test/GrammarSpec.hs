{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
module GrammarSpec(spec) where

import Test.Hspec
import Grammar(parseText)
import Syntax(Expression(..),
           WithContext(..),
           Identifier,
           GateApp(..),
           NonNeg(..),
           NatNum,
           RegisterType(..),
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
toGateWithinCommand :: LineNumber -> String -> [Expression] -> Command
toGateWithinCommand line gateName'= Gate . App (WithContext gateName' line)

gateOnLine1 = toGateWithinCommand (LineNumber 1)

line1 :: LineNumber
line1 = LineNumber 1

onLine1 :: a -> WithContext a LineNumber
onLine1 = flip WithContext line1

regAccess :: Identifier -> Int -> Expression
regAccess regCollname idx = RegisterAccess (onLine1 regCollname) (onLine1 (NonNeg idx))

index :: Int -> NatNum
index = onLine1 . NonNeg

-- Takes the name of a variable and
-- generates the corresponding MetaQASM term for
-- a variable found on the first line of a MetaQASM program
var :: String -> Expression
var = Var . onLine1

-- Takes a program representing a MetaQASM expression, the expression that should
-- be obtained after parsing the program, and checks that the expected expression
-- is equivalent to the parsed program
shouldParseToExpr :: MetaQasmProgram -> Expression -> Expectation
shouldParseToExpr text expected = (fmap toExpr . parseText) text `shouldBe` Right expected

regCollDecl :: RegisterType -> String -> Int -> Command -> Command
regCollDecl collType regCollName regCount innerExpr = DeclRegCollIn{collType, regCollName, numOfRegs = index regCount, innerExpr }

quantumRegCollDecl = regCollDecl Quantum
classicalRegCollDecl = regCollDecl Classical

spec :: Spec

spec = do
  describe "Parsing MetaQASM programs" $ do
    describe "Parsing variables" $ do
      it "Generates a variable with the context of where it was found" $ do
        "varName" `shouldParseToExpr` var "varName"

    describe "Parsing register accesses" $ do
      it "Generates a register access with the context of where a register collection was accessed" $ do
        "regColl[1]" `shouldParseToExpr` regAccess "regColl" 1

    describe "Parsing qubit measurements" $ do
      it "Generates a term representing the act of measuring a qubit" $ do
        "measure q -> b" `shouldParseToCommand` MeasureQubit{toMeasure = var "q", toStoreIn = var "b"}

    describe "Parsing locally scoped register collection declarations" $ do
      it "Generates a term with the context of where the collections and inner expressions were declared" $ do
        "creg regColl[1] in {h(x)}" `shouldParseToCommand` classicalRegCollDecl "regColl" 1 (gateOnLine1 "h" [var "x"])

        "qreg regColl[1] in {h(x)}" `shouldParseToCommand` quantumRegCollDecl "regColl"  1 (gateOnLine1 "h" [var "x"])

    describe "Parsing gate applications" $
      it "Generates a term representing the application" $ do
        "tdg(varName)" `shouldParseToCommand` (gateOnLine1 "tdg") [var "varName"]
        "h(varName)" `shouldParseToCommand` (gateOnLine1 "h" ) [var "varName"]
        "t(varName)" `shouldParseToCommand` (gateOnLine1 "t" ) [var "varName"]
        "cx(var1, var2)" `shouldParseToCommand` (gateOnLine1 "cx") [var "var1", var "var2"]

    describe "Parsing scoped gate declarations" $ do
      describe "Parsing declarations with qubit arguments" $  do
        it "Generates a term representing the declaration and its application" $ do
          let expectedGateArgs = [GateArg "x" Qbit, GateArg "y" Qbit]
          let cnot = onLine1 "cx"
          let expectedGateBody = App cnot [var "x" , var "y"]
          let fnName = onLine1 "f"
          let expectedGateApp = Gate (App fnName [regAccess "c" 0,
                                                  regAccess "c" 1])
          let expectedInnerExpr = quantumRegCollDecl "c" 2 expectedGateApp
          "gate f(x: Qbit, y: Qbit) {cx(x, y)} in {qreg c[2] in {f(c[0], c[1])}}" `shouldParseToCommand` DeclGateIn "f" expectedGateArgs expectedGateBody expectedInnerExpr

      describe "Parsing declarations with bit arguments" $ do
        it "Generates a MetaQASM term representing the declaration and subsequent application" $ do
          let expectedGateArgs = [GateArg "y" Bit]
          let gateBody = App (onLine1 "h") [var "y"]
          let gate = onLine1 "f"
          let gateApp = Gate (App gate [var "a"])
          "gate f(y: Bit) {h(y)} in {f(a)}" `shouldParseToCommand` DeclGateIn "f" expectedGateArgs gateBody gateApp

