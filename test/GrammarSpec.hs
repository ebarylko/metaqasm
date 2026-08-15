{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
module GrammarSpec(spec) where

import Test.Hspec
import Grammar(parseText)
import Syntax(Expression(..),
           WithContext(..),
           Identifier,
           RegCollInfo(..),
           toConstIdx,
           GateInfo(..),
           GateApp(..),
           Index(..),
           Idx,
           IndexVar(..),
           RegisterType(..),
           Command(..),
           GateArg(..),
           TermType(..))
import Lexer(LineNumber(..))
import qualified Vary
import Data.Maybe(fromJust)
import Generators (MetaQasmProgram)
import Typecheck(Term)
import Control.Arrow((>>>))
import Data.Function(on)
import Data.Map as M

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
toGateWithinCommand line gateName'= Gate . GateApp (WithContext gateName' line)

gateApp :: Identifier -> [Expression] -> GateApp
gateApp gateId gateArgs = GateApp (gate gateId) gateArgs
  where
    gate = onLine1


gateOnLine1 = toGateWithinCommand (LineNumber 1)

line1 :: LineNumber
line1 = LineNumber 1

onLine1 :: a -> WithContext a LineNumber
onLine1 = flip WithContext line1

regAccess :: Identifier -> Int -> Expression
regAccess regCollname idx = RegisterAccess (onLine1 regCollname) (index idx)

index :: Int -> Idx
index = onLine1 . toConstIdx

-- Takes an binary operation on indices, two numbers representing
-- the indices, and applies the operations on the index equivalent
-- form of the numbers
binOpOnIndices :: (Index -> Index -> Index) -> Int -> Int -> Idx
binOpOnIndices op arg = (op `on` toConstIdx) arg >>> onLine1

regCollAccessThatUsesBinOpOnIndices :: (Index -> Index -> Index) -> Identifier -> Int -> Int -> Expression
regCollAccessThatUsesBinOpOnIndices op regCollName fstIdx  = binOpOnIndices op fstIdx >>> RegisterAccess (onLine1 regCollName)

-- Takes the name of a register collection, indices x and y, and
-- generates an expression representing the access of the
-- (x + y)th element of the collection
indexSumRegAccess :: Identifier -> Int -> Int -> Expression
indexSumRegAccess = regCollAccessThatUsesBinOpOnIndices (+)

-- Takes the name of a register collection, indices x and y, and
-- generates an expression representing the access of the
-- (x - y)th element of the collection
indexDiffRegAccess :: Identifier -> Int -> Int -> Expression
indexDiffRegAccess = regCollAccessThatUsesBinOpOnIndices (-)

toIndexVar :: Identifier -> Index
toIndexVar = IndexVar >>> flip M.singleton 1 >>> Index 0

-- Takes the name of a register collection, an index variable n,
--  a number N, a function that generates an index  i,
-- and returns a register access for the ith element
indexDiffRegAccessUsing :: (Identifier -> Int -> Index) -> Identifier -> Identifier -> Int -> Expression

indexDiffRegAccessUsing f regCollName indexVarName idx = RegisterAccess (onLine1 regCollName) $ onLine1 $ f indexVarName idx

subNumFromIndexVar = indexDiffRegAccessUsing $ \indexVarName idx -> Index (-idx) (M.singleton (IndexVar indexVarName) 1) 
subIndexVarFromNum = indexDiffRegAccessUsing $ \indexVarName idx -> Index idx (M.singleton (IndexVar indexVarName) (-1)) 

-- Takes the name of a register collection, an index variable n,
-- a number N, and returns a register access for the
-- (n - N)th element
indexDiffRegAccess' :: Identifier -> Identifier -> Int -> Expression
indexDiffRegAccess' regCollName indexVarName sndIdx = RegisterAccess (onLine1 regCollName) $ onLine1 $ Index (-sndIdx) (M.singleton (IndexVar indexVarName) 1)


indexDiffRegAccess'' :: Identifier -> Int -> Identifier -> Expression
indexDiffRegAccess'' regCollName fstIdx indexVarName = RegisterAccess (onLine1 regCollName) $ onLine1 $ Index fstIdx (M.singleton (IndexVar indexVarName) (-1))

-- Takes the name of a register collection, indices x and y, and
-- generates an expression representing the access of the
-- (x * y)th element of the collection
indexProdRegAccess ::  Identifier -> Int -> Int -> Expression
indexProdRegAccess = regCollAccessThatUsesBinOpOnIndices (*)

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

regCollInfo :: RegisterType -> String -> Int -> RegCollInfo
regCollInfo collType regCollName regCount = RegCollInfo collType regCollName (index regCount)

regCollDecl :: RegisterType -> String -> Int  -> Command

regCollDecl collType regCollName regCount  =  RegCollDecl $ regCollInfo collType regCollName regCount

scopedRegCollDecl :: RegisterType -> String -> Int -> Command -> Command
scopedRegCollDecl collType regCollName regCount innerExpr = ScopedRegCollDecl (regCollInfo collType regCollName regCount) innerExpr

scopedQuantumRegCollDecl = scopedRegCollDecl Quantum
scopedClassicalRegCollDecl = scopedRegCollDecl Classical

-- Takes the kind of the register collection k, the name of the collection n,
-- the number of elements in the collection N, and generates a type annotation noting that
-- n is a constant N sized register collection of kind k
regCollAnnotation ::  RegisterType -> Identifier  -> Int -> GateArg
regCollAnnotation collKind collName = index >>> RegisterGroup collKind >>> GateArg collName
quantumRegColl  = regCollAnnotation Quantum
classicalRegColl = regCollAnnotation Classical


-- Takes the name of the register collection n, the family variable
-- indicating the size of the collection, and generates a
-- type annotation noting that n is a parametric size quantum collection
parametricQuantRegColl :: Identifier -> Identifier -> GateArg
parametricQuantRegColl collName =  indexVar >>> RegisterGroup Quantum >>> GateArg collName
  where
    indexVar = IndexVar >>> flip M.singleton 1 >>> Index 0 >>> onLine1

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
        "measure q -> b" `shouldParseToCommand` QubitMeasurement{toMeasure = var "q", toStoreIn = var "b"}

    describe "Parsing locally scoped register collection declarations" $ do
      it "Generates a term with the context of where the collections and inner expressions were declared" $ do
        "creg regColl[1] in {h(x)}" `shouldParseToCommand` scopedClassicalRegCollDecl "regColl" 1 (gateOnLine1 "h" [var "x"])

        "qreg regColl[1] in {h(x)}" `shouldParseToCommand` scopedQuantumRegCollDecl "regColl"  1 (gateOnLine1 "h" [var "x"])

    describe "Parsing gate applications" $
      it "Generates a term representing the application" $ do
        "tdg(varName)" `shouldParseToCommand` (gateOnLine1 "tdg") [var "varName"]
        "h(varName)" `shouldParseToCommand` (gateOnLine1 "h" ) [var "varName"]
        "t(varName)" `shouldParseToCommand` (gateOnLine1 "t" ) [var "varName"]
        "cx(var1, var2)" `shouldParseToCommand` (gateOnLine1 "cx") [var "var1", var "var2"]

    describe "Parsing scoped gate declarations" $ do
      it "Generates a term representing the declaration and its application" $ do
        let expectedGateArgs = [GateArg "x" Qbit,
                                GateArg "z" Bit,
                                quantumRegColl "y" 2]
        let expectedGateBody = gateApp "cx" [var "x" , var "z"]
        let expectedGateApp = Gate $ gateApp "f" [var "a", var "b"]
        "gate f(x: Qbit, z: Bit, y: Qbit[2]) {cx(x, z)} in {f(a, b)}" `shouldParseToCommand` ScopedGateDecl (GateInfo "f" expectedGateArgs expectedGateBody) expectedGateApp

    describe "Parsing qubit resets" $ do
      it "Generates a term representing the act of setting a qubit to its default state" $ do
        "reset x" `shouldParseToCommand` QubitReset (var "x")

    describe "Parsing non-scoped register collection declarations" $ do
      it "Generates a term representing the declaration" $ do
        "qreg x[1]" `shouldParseToCommand` regCollDecl Quantum "x" 1
        "creg x[1]" `shouldParseToCommand` regCollDecl Classical "x" 1

    describe "Parsing sequences of commands" $ do
      it "Generates a new command where the command on the left is executed before that on the right" $ do
        let fstRegCollDecl = regCollDecl Quantum "x" 1
        let sndRegCollDecl = regCollDecl Quantum "y" 1
        "qreg x[1] ; qreg y[1]; qreg x[1]" `shouldParseToCommand` Sequence fstRegCollDecl (Sequence sndRegCollDecl fstRegCollDecl)

    describe "Parsing unscoped gate declarations" $ do
      it "Generates a term representing the declaration" $ do
        "gate f(h: Circuit(Qbit, Qbit)) {h(y, y)}" `shouldParseToCommand` GateDecl (GateInfo
                                                                                    "f"
                                                                                    [GateArg "h" $ Circuit [Qbit, Qbit]]
                                                                                    (gateApp "h" [var "y", var "y"]))
        "gate f(y: Bit[2]) {h(y)} " `shouldParseToCommand` GateDecl ( GateInfo
                                                                      "f"
                                                                      [classicalRegColl "y" 2]
                                                                      (gateApp "h" [var "y"]))

    describe "Parsing gate sequencing" $ do
      it "Generates a term representing the combining of gates" $ do
        "gate f(x: Qbit) {h(x) ; t(x)}" `shouldParseToCommand` GateDecl (GateInfo
                                                                         "f"
                                                                         [GateArg "x" Qbit]
                                                                         (GateSequence (gateApp "h" [var "x"]) (gateApp "t" [var "x"])))

    describe "Parsing a conditional gate execution" $ do
      it "Generates a term representing the execution of a gate contingent on the guard" $ do
        "if (x == 1) {h(x)}" `shouldParseToCommand` ConditionalGateExec (var "x") (gateApp "h" [var "x"])

    describe "Parsing binary operations on indices" $ do
      describe "Summing two indices" $ do
        it "Yields a term representing the summation" $ do
          "x[0 + 0]" `shouldParseToExpr` indexSumRegAccess "x" 0 0

      describe "Taking the difference of two indices" $ do
        it "Yields a term representing the difference" $ do
          "x[0 - 0]" `shouldParseToExpr` indexDiffRegAccess "x" 0 0
          "x[n - 1]" `shouldParseToExpr` subNumFromIndexVar "x" "n" 1
          "x[1 - n]" `shouldParseToExpr` subIndexVarFromNum "x" "n" 1
          "x[n - n]" `shouldParseToExpr` indexDiffRegAccess "x" 0 0

      describe "Taking the product of two indices" $ do
        it "Yields a term representing the product" $ do
          "x[2 * 3 - 1]" `shouldParseToExpr` indexProdRegAccess "x" 5 1


    describe "Parsing circuit family declarations" $ do
      it "Generates a term representing a parameterized circuit" $ do
        "family (n, y) f(coll : Qbit[1]) {h(x)}" `shouldParseToCommand`  GateFamilyDecl [IndexVar "n", IndexVar "y"] (GateInfo "f" [quantumRegColl "coll" 1] (gateApp "h" [var "x"]))
        "family (n) f(coll : Qbit[n]) {h(x)}" `shouldParseToCommand`  GateFamilyDecl [IndexVar "n"] (GateInfo "f" [parametricQuantRegColl "coll" "n"] (gateApp "h" [var "x"]))
