{-# LANGUAGE NamedFieldPuns #-}

module Typecheck
    ( Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      TypeCalculationResult,
      RegisterType(..),
      TypeError(..),
      Nat(..)
    ) where

import qualified Data.Map as M
import Data.Function ((&))
import Control.Monad (mfilter)

-- This data type represents a natural number
newtype Nat = Nat Int deriving (Eq, Show, Ord)

type Index = Nat

type Identifier = String

-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType

-- This data type represents the values an expression can take on,
-- being either a reference to another term or an attempt to obtain a bit or qubit from a
-- collection of registers
data Expression = Identifier | RegisterAccess{registerName::Identifier,  registerNumber::Nat} deriving (Show, Eq)

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

-- This data type represents the possible types a term can take on, being a classical bit,
-- a quantum bit, or a collection of classical/quantum registers of size N, where N >= 0
data TermType
  = Bit
  | Qbit
  | Registers RegisterType Nat
  deriving (Show, Eq)

data TypeError = UsesInvalidArrayIndex deriving (Show, Eq)

-- This type represents the result of determining the type of an
-- expression, being either a valid type or one that is invalid due to one or more reasons.
type TypeCalculationResult = Either TypeError TermType

-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Expression -> TypeCalculationResult
determineType m (RegisterAccess{registerName, registerNumber}) = M.lookup registerName m & mfilter (isAccessingValidReg registerNumber) & maybe (Left UsesInvalidArrayIndex) getRegisterContentType
  where
    isAccessingValidReg :: Index -> TermType -> Bool
    isAccessingValidReg registerIdx (Registers _ regCount) = regCount > registerIdx
    isAccessingValidReg _ _ = False


    getRegisterContentType :: TermType -> TypeCalculationResult
    getRegisterContentType (Registers Quantum _) = Right Qbit


determineType _ _ = undefined
