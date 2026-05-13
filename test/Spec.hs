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
  x@(RegisterGroupInfo _ (Pos v)) <- registerGroupInfo
  randIdx <- chooseInt (0, v - 1)
  (return . Spec x) $  Nat randIdx


instance Arbitrary ValidRegAccessSpec where
  arbitrary = validRegAccessSpec


-- Tests that accessing the ith register from a collection of registers of
-- size N, where N > i, always succeeds
prop_regAccessAlwaysValid  (Spec specInfo@(RegisterGroupInfo regType _) regIdx) =
  determineType (genContext [("x", RegisterGroup specInfo)]) (accessNthRegister "x" regIdx) `shouldBe` expectedRegisterContent
  where

    -- Takes the name of the registers to access, I, the index
    -- of the wanted register, n, and returns a request to
    -- access the nth register of I
    accessNthRegister :: Identifier -> Nat -> Expression
    accessNthRegister name regIdx = RegisterAccess{registerName = name, registerNumber = regIdx}

    calcContentType :: RegisterType -> TermType
    calcContentType Classical = Bit
    calcContentType Quantum = Qbit

    expectedRegisterContent = (Right . calcContentType) regType

-- This data type represents a specification describing a collection
-- of quantum/classical registers of size N such that accessing the
-- ith register, i > N, is a invalid operation
data InvalidRegAccessSpec = Info RegisterGroupInfo Nat deriving (Show, Eq)

invalidRegAccessSpec :: Gen InvalidRegAccessSpec

invalidRegAccessSpec = do
  x@(RegisterGroupInfo _ (Pos v)) <- registerGroupInfo
  randIdx <- chooseInt (v, v + 50)
  (return . Info x) $  Nat randIdx


instance Arbitrary InvalidRegAccessSpec where
  arbitrary = invalidRegAccessSpec

-- Tests that accessing the ith register from a collection of registers of
-- size N, where N <= i, always fails
prop_regAccessAlwaysFails  (Info specInfo regIdx) =
  determineType (genContext [("x", RegisterGroup specInfo)]) (accessNthRegister "x" regIdx) `shouldBe` Left UsesInvalidArrayIndex
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

    prop "Accessing the ith register where i >= N returns an error" $ do
      prop_regAccessAlwaysFails
