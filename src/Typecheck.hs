{-# LANGUAGE NamedFieldPuns #-}

module Typecheck
    ( Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      TypeCalculationResult,
      RegisterType(..),
      TypeEvaluationError(..),
      Nat(..),
      Pos(..),
      Index,
      MisMatch(..)
    ) where

import qualified Data.Map as M
import Data.Function ((&))
import Control.Arrow ((>>>))
import Data.List.NonEmpty


-- This data type represents a natural number
newtype Nat = Nat Int deriving (Eq, Show, Ord)

type Index = Nat

type Identifier = NonEmpty Char

-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType


-- This data type represents the values an expression can take on,
-- being either a reference to another term or an attempt to obtain a bit or qubit from a
-- collection of registers
data Expression = Identifier | RegisterAccess{registerName::Identifier,  registerNumber::Nat} deriving (Show, Eq)

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

newtype Pos = Pos Int deriving (Show, Eq, Ord)

data TermType
  = Bit
  | Qbit
  | RegisterGroup RegisterType Pos
  deriving (Show, Eq)

-- This data type represents the ways there can be a mismatch between an expected and actual type
data MisMatch = ExpectedRegColl {minCollSize :: Pos} deriving (Show, Eq)

-- This data type represents all the possible reasons for why the type of an expression cannot be
-- determined
data TypeEvaluationError = UsesInvalidArrayIndex | VariableNotInScope Identifier | TypeMismatch{varName :: Identifier, actualVarType:: TermType, whatWentWrong :: MisMatch} deriving (Show, Eq)

-- This type represents the result of determining the type of an
-- expression, being either a valid type or one that is invalid due to one or more reasons.
type TypeCalculationResult = Either TypeEvaluationError TermType

-- Takes an id referring to an expression, an evaluation scope, and returns the type of the referenced
-- expression if it exists. Returns an error otherwise.
findTypeWithinScope :: Identifier -> EvaluationContext -> TypeCalculationResult

findTypeWithinScope varName = M.lookup varName >>> maybe (Left $ VariableNotInScope varName) Right

eitherFromPred :: (a -> Bool) -> (a -> err) -> a -> Either err a

eitherFromPred predicate elseCase x = if predicate x then Right x else (Left . elseCase) x

incNat  :: Nat -> Nat

incNat (Nat v) = Nat $ v + 1

unsafeNatToPos :: Nat -> Pos

unsafeNatToPos (Nat v) = Pos v

-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Expression -> TypeCalculationResult
determineType m (RegisterAccess{registerName, registerNumber}) =
  findTypeWithinScope registerName m
  >>= eitherFromPred isAccessingRegColl genMismatchInfo
  >>= eitherFromPred (isAccessingValidReg registerNumber) (const UsesInvalidArrayIndex)
  & fmap getRegisterContentType
  where
    isAccessingValidReg :: Index -> TermType -> Bool
    isAccessingValidReg (Nat registerIdx) (RegisterGroup _ (Pos regCount)) = regCount > registerIdx

    getRegisterContentType :: TermType -> TermType
    getRegisterContentType (RegisterGroup Quantum _ ) = Qbit
    getRegisterContentType (RegisterGroup Classical _ ) = Bit

    isAccessingRegColl :: TermType -> Bool
    isAccessingRegColl (RegisterGroup _ _)  = True
    isAccessingRegColl _  = False

    minSize = unsafeNatToPos . incNat $ registerNumber
    genMismatchInfo = flip (TypeMismatch registerName)  (ExpectedRegColl minSize)


determineType _ _ = undefined
