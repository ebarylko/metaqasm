module Typecheck
    ( Expression(..),
      EvaluationContext,
      determineType,
      Identifier,
      TermType(..),
      TypeCalculationResult
    ) where

import qualified Data.Map as M

{-@ type Index  = Nat @-}

type Identifier = String

type EvaluationContext = M.Map Identifier TermType

-- This data type represents the values an expression can take on,
-- being either a reference to another term or an attempt to obtain a bit or qubit from a
-- collection of registers
{-@ data Expression = Identifier | RegisterAccess{registerName::Identifier,  registerNumber::Index} @-}
data Expression = Identifier | RegisterAccess{registerName::Identifier,  registerNumber::Int} deriving (Show, Eq)

{-@ newtype RegistersInfo = RegistersInfo{registerName:: Identifier, numOfRegisters:: NonNegative} deriving (Show, Eq) @-}

-- This data type represents the possible commands a user can execute, including the creation of
-- n classical or quantum registers accessible under a certain name
{-@  data Command = DeclareQuantumRegisters RegistersInfo | DeclareClassicalRegisters RegistersInfo deriving (Show, Eq) @-}


data TermType
  = Bit
  | Qbit
  | QuantumRegisters Int
  | ClassicalRegisters Int
  deriving (Show, Eq)

{-@
data TermType
  = Bit
  | Qbit
  | QuantumRegisters { numQRegs :: Nat }
  | ClassicalRegisters { numCRegs :: Nat }
  deriving (Show, Eq)
@-}

data TypeError = UsesInvalidArrayIndex deriving (Show, Eq)

-- This type represents the result of determining the type of an
-- expression, being either a valid type or one that is invalid due to one or more reasons.
type TypeCalculationResult = Either TypeError TermType

-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Expression -> TypeCalculationResult
determineType _ _ = undefined
