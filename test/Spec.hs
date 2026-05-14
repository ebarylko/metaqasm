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
      RegisterGroupInfo(..),
      Index
      )

import qualified Data.Map as M
import Test.QuickCheck 
import Test.Hspec.QuickCheck
import Data.List.NonEmpty as NL

-- Takes an association list of identifiers to their respective types and
-- returns an equivalent context to evaluate expressions
genContext :: [(Identifier, TermType)] -> EvaluationContext

genContext = M.fromList

positiveNum :: Gen Pos
positiveNum = (Pos . getPositive) <$> (arbitrary :: Gen (Positive Int))

registerType :: Gen RegisterType
registerType = oneof [return Classical, return Quantum]


registerGroupInfo :: Gen RegisterGroupInfo
registerGroupInfo = RegisterGroupInfo <$> registerType <*> positiveNum

instance Arbitrary RegisterGroupInfo where
  arbitrary = registerGroupInfo

-- This data type represents a specification describing a collection
-- of quantum/classical registers of size N and a request to
-- access the ith register, where i >= 0
data RegAccessSpec = Spec RegisterGroupInfo Nat deriving (Show, Eq)



-- Takes a register collection id A, the index of the register to
-- access I, and returns an expression representing a request to
-- access the Ith register of A
accessNthRegister :: Identifier -> Nat -> Expression
--accessNthRegister name regIdx = RegisterAccess{registerName = name, registerNumber = regIdx}
accessNthRegister = RegisterAccess 


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NL.fromList  <$> listOf1 arbitrary

prop_regAccessAlwaysValid :: RegAccessSpec -> Identifier -> IO ()

-- Tests that accessing the ith register from a collection of registers of
-- size N, where N > i, always succeeds
prop_regAccessAlwaysValid  (Spec specInfo@(RegisterGroupInfo regType _) regIdx) regName =
  determineType (genContext [(regName, RegisterGroup specInfo)]) (accessNthRegister regName regIdx) `shouldBe` expectedRegisterContent
  where

    expectedRegisterContent = (Right . calcContentType) regType

    calcContentType :: RegisterType -> TermType
    calcContentType Classical = Bit
    calcContentType Quantum = Qbit


prop_regAccessAlwaysFails :: RegAccessSpec -> Identifier -> IO ()


-- Tests that accessing the ith register from a collection of registers of
-- size N, where N <= i, always fails
prop_regAccessAlwaysFails  (Spec specInfo regIdx) regName =
  determineType (genContext [(regName, RegisterGroup specInfo)]) (accessNthRegister regName regIdx) `shouldBe` Left UsesInvalidArrayIndex

-- Takes a function that modifies range of registers accessed
-- in a specification and returns a generator that uses
-- the function to determine which registers will be accessed
genRegAccessSpec :: (Int -> (Int, Int)) -> Gen RegAccessSpec

genRegAccessSpec f = do
  x@(RegisterGroupInfo _ (Pos v)) <- registerGroupInfo
  randIdx <- (chooseInt . f) v
  (return . Spec x) $  Nat randIdx

-- Generates specifications where accessing the
-- ith register is safe
validRegAccessSpec :: Gen RegAccessSpec

validRegAccessSpec = genRegAccessSpec $ \x -> (0, x - 1)

-- Generates specifications where accessing the
-- ith register is unsafe
invalidRegAccessSpec :: Gen RegAccessSpec

invalidRegAccessSpec = genRegAccessSpec $ \x -> (x, x + 50)

instance Arbitrary Nat where
  arbitrary = Nat . getNonNegative <$> (arbitrary :: Gen (NonNegative Int))

prop_cannotAccessOutOfScopeRegColl :: Identifier -> Index -> IO ()

prop_cannotAccessOutOfScopeRegColl regName regIdx =
  determineType emptyCtx (accessNthRegister regName regIdx) `shouldBe` (Left . VariableNotInScope) regName
  where
    emptyCtx = M.empty


main :: IO ()
main = hspec $ do
  describe "Accessing elements from a collection of registers of size N > 0" $ do
    prop "Accessing the ith register where i is in [0, N) returns the content inside the register" $ do
      forAll validRegAccessSpec prop_regAccessAlwaysValid 

    prop "Accessing the ith register where i >= N returns an error" $ do
      forAll invalidRegAccessSpec  prop_regAccessAlwaysFails 

  describe "Accessing elements from a collection of registers that is out of scope" $ do
    prop "Accessing any register returns an error stating the collection is not in scope" $ do
      prop_cannotAccessOutOfScopeRegColl
