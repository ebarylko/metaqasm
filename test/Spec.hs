import Test.Hspec
import Typecheck(Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      RegisterType(..),
      TypeError(..),
      Nat(..),
      Positive(..)
      )
import qualified Data.Map as M

-- Takes an association list of identifiers to their respective types and
-- returns an equivalent context to evaluate expressions
genContext :: [(Identifier, TermType)] -> EvaluationContext

genContext = M.fromList

genNQuantumRegisters :: Int -> TermType

genNQuantumRegisters = Registers Quantum . Positive

-- Takes the name of the registers to access, I, the index
-- of the wanted register, n, and returns a request to
-- access the nth register of I
accessNthRegister :: Identifier -> Int -> Expression

accessNthRegister name regIdx = RegisterAccess{registerName = name, registerNumber = Nat regIdx}


-- positiveNum = arbitrarySizedNatural `suchThat` (0 `<`)

main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers" $ do
    describe "Using a valid index to access a register" $ do
      it "Returns the content inside the register" $ do
        determineType (genContext [("x", genNQuantumRegisters 2)]) (accessNthRegister "x" 1) `shouldBe` Right Qbit
        determineType (genContext [("x", genNQuantumRegisters 2)]) (accessNthRegister "x" 1) `shouldBe` Right Qbit

    describe "Using an index outside of the bounds of the registers" $ do
      it "Returns an invalid index error" $ do
        determineType (genContext [("x", genNQuantumRegisters 2)]) (accessNthRegister "x" 2) `shouldBe` Left UsesInvalidArrayIndex
        --determineType (genContext [("x", Registers Quantum 2)]) RegisterAccess{registerName = "x", registerNumber = (-1)} `shouldBe` Left UsesInvalidArrayIndex
