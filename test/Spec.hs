import Test.Hspec
import Typecheck(Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      RegisterType(..),
      TypeError(..)
      )
import qualified Data.Map as M

-- Takes an association list of identifiers to their respective types and
-- returns an equivalent context to evaluate expressions
genContext :: [(Identifier, TermType)] -> EvaluationContext

genContext = M.fromList

main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers" $ do
    describe "Using a valid index to access a register" $ do
      it "Returns the content inside the register" $ do
        determineType (genContext [("x", Registers Quantum 2)]) RegisterAccess{registerName = "x", registerNumber = 0} `shouldBe` Right Qbit

    describe "Using an index outside of the bounds of the registers" $ do
      it "Returns an invalid index error" $ do
        determineType (genContext [("x", Registers Quantum 2)]) RegisterAccess{registerName = "x", registerNumber = 2} `shouldBe` Left UsesInvalidArrayIndex
