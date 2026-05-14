{-# OPTIONS_GHC -Wno-orphans #-}
import Test.Hspec
import Typecheck(Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      RegisterType(..),
      TypeEvaluationError(..),
      Nat(..),
      Pos(..),
      Index,
      Mismatch(..)
      )

import qualified Data.Map as M
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.List.NonEmpty as NL

-- Takes an identifier, the type it is associated with, and
-- returns a minimal non-empty context 
genMinContext :: Identifier -> TermType -> EvaluationContext

genMinContext = M.singleton



instance Arbitrary RegisterType where
  arbitrary = elements [Classical, Quantum]

instance Arbitrary Pos where
  arbitrary = Pos . getPositive <$> arbitrary

-- This type represents the information needed to create a collection of
-- registers, being the type of the registers and the number of registers.
type RegisterGroupInfo = (RegisterType, Pos)

-- This data type represents a specification describing a collection
-- of quantum/classical registers of size N and a request to
-- access the ith register, where i >= 0
data RegAccessSpec = Spec RegisterGroupInfo Nat deriving (Show, Eq)


-- Takes a register collection id A, the index of the register to
-- access I, and returns an expression representing a request to
-- access the Ith register of A
accessNthRegister :: Identifier -> Nat -> Expression
accessNthRegister = RegisterAccess


instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NL.fromList  <$> listOf1 arbitrary

prop_regAccessAlwaysValid :: RegAccessSpec -> Identifier -> IO ()

-- Tests that accessing the ith register from a collection of registers of
-- size N, where N > i, always succeeds
prop_regAccessAlwaysValid  (Spec info@(regType, _) regIdx) regName =
  determineType ctx regAccReq `shouldBe` expectedRegisterContent
  where

    expectedRegisterContent = (Right . calcContentType) regType

    calcContentType :: RegisterType -> TermType
    calcContentType Classical = Bit
    calcContentType Quantum = Qbit

    ctx = genMinContext regName (uncurry RegisterGroup info)
    regAccReq = accessNthRegister regName regIdx


prop_regAccessAlwaysFails :: RegAccessSpec -> Identifier -> IO ()


-- Tests that accessing the ith register from a collection of registers of
-- size N, where N <= i, always fails
prop_regAccessAlwaysFails  (Spec info regIdx) regName =
  determineType ctx regAccReq `shouldBe` Left UsesInvalidArrayIndex
  where
    ctx = genMinContext regName (uncurry RegisterGroup info)
    regAccReq = accessNthRegister regName regIdx

-- Takes a function that modifies range of registers accessed
-- in a specification and returns a generator that uses
-- the function to determine which registers will be accessed
genRegAccessSpec :: (Int -> (Int, Int)) -> Gen RegAccessSpec

genRegAccessSpec f = do
  x@(_, (Pos v)) <- arbitrary
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
  arbitrary = Nat . getNonNegative <$> arbitrary 

-- Tests that accessing a register collection that is not in
-- the current evaluation scope always fails.
prop_cannotAccessOutOfScopeRegColl :: Identifier -> Index -> IO ()

prop_cannotAccessOutOfScopeRegColl regName regIdx =
  determineType emptyCtx regAccReq `shouldBe` (Left . VariableNotInScope) regName
  where
    emptyCtx = M.empty
    regAccReq = accessNthRegister regName regIdx

-- This tests that expressions not evaluating to a register collection
-- cannot be indexed.
prop_canOnlyIndexARegColl :: TermType -> Identifier -> Index -> IO ()

prop_canOnlyIndexARegColl varType varID regIdx@(Nat v) =
  determineType ctx regAccReq `shouldBe` Left (TypeMismatch varID varType mismatch)
  where
    ctx = genMinContext varID varType
    regAccReq = accessNthRegister varID regIdx

    mismatch = ExpectedAtLeastNRegs . Pos . (+ 1) $ v

nonRegCollType :: Gen TermType
nonRegCollType = elements [Bit, Qbit]

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

  describe "Accessing a register from an expression that does not evaluate to a register collection" $ do
    prop "Accessing a register returns an error noting the type mismatch" $ do
      forAll nonRegCollType prop_canOnlyIndexARegColl
