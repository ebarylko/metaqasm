import Test.Hspec
import Typecheck(Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      RegisterType(..)
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
