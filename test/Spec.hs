import Test.Hspec
import Typecheck(Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      RegisterType(..),
      TypeError(..),
      Nat(..),
      Pos(..),
      RegisterGroupInfo(..)
      )

import qualified Data.Map as M
import Test.QuickCheck 
import Test.Hspec.QuickCheck

-- Takes an association list of identifiers to their respective types and
-- returns an equivalent context to evaluate expressions
genContext :: [(Identifier, TermType)] -> EvaluationContext

genContext = M.fromList

genNQuantumRegisters :: Int -> TermType

genNQuantumRegisters = RegisterGroup . RegisterGroupInfo Quantum . Pos


positiveNum :: Gen Pos
positiveNum = toPos <$> (arbitrary :: Gen (Positive Int))
  where
    toPos (Positive n) = Pos n

registerType :: Gen RegisterType
registerType = oneof [return Classical, return Quantum]


registerGroupInfo :: Gen RegisterGroupInfo
registerGroupInfo = RegisterGroupInfo <$> registerType <*> positiveNum

instance Arbitrary RegisterGroupInfo where
  arbitrary = registerGroupInfo

-- This data type represents a specification describing a collection
-- of quantum/classical registers of size N such that accessing the
-- ith register, i in [0, N), is a valid operation
data ValidRegAccessSpec = Spec RegisterGroupInfo Nat deriving (Show, Eq)

validRegAccessSpec :: Gen ValidRegAccessSpec

validRegAccessSpec = do
  x@(RegisterGroupInfo regType numOfRegs@(Pos v)) <- registerGroupInfo
  randIdx <- chooseInt (0, v - 1)
  (return . Spec x) $  Nat randIdx


instance Arbitrary ValidRegAccessSpec where
  arbitrary = validRegAccessSpec


prop_regAccessAlwaysValid  (Spec specInfo@(RegisterGroupInfo regType _) regIdx) =
  determineType (genContext [("x", RegisterGroup specInfo)]) (accessNthRegister "x" regIdx) `shouldBe` (Right . calcContentType) regType
  where

    -- Takes the name of the registers to access, I, the index
    -- of the wanted register, n, and returns a request to
    -- access the nth register of I
    accessNthRegister :: Identifier -> Nat -> Expression
    accessNthRegister name regIdx = RegisterAccess{registerName = name, registerNumber = regIdx}

    calcContentType :: RegisterType -> TermType
    calcContentType Classical = Bit
    calcContentType Quantum = Qbit





main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers of size N > 0" $ do
    prop "Accessing the ith register where i is in [0, N) returns the content inside the register" $ do
      prop_regAccessAlwaysValid
--        determineType (genContext [("x", genNQuantumRegisters 2)]) (accessNthRegister "x" 1) `shouldBe` Right Qbit
--        determineType (genContext [("x", genNQuantumRegisters 2)]) (accessNthRegister "x" 1) `shouldBe` Right Qbit

--    describe "Using an index outside of the bounds of the registers" $ do
--      it "Returns an invalid index error" $ do
--        determineType (genContext [("x", genNQuantumRegisters 2)]) (accessNthRegister "x" 2) `shouldBe` Left UsesInvalidArrayIndex
--        --determineType (genContext [("x", Registers Quantum 2)]) RegisterAccess{registerName = "x", registerNumber = (-1)} `shouldBe` Left UsesInvalidArrayIndex
