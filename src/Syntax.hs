module Syntax(Expression(..),
          WithContext(..),
          Identifier,
          Index,
          Idx,
          Id,
          GateApp(..),
          NatNum,
          TermType(..),
          RegisterType(..),
          NonNeg(..),
          Command(..)) where

import Lexer(LineNumber)

type Identifier = String

-- This data type represents a value along with its associated
-- context, e.g., where the file was found, the type of the value, etc.
data WithContext a ctx = WithContext a ctx deriving (Eq, Show)

type Id = WithContext Identifier LineNumber

-- Represents a nonnegative number
newtype NonNeg = NonNeg Int deriving (Eq, Show, Ord)

type Index = NonNeg

type Idx = WithContext Index LineNumber

-- This data type represents the values an expression can take on,
-- being either a reference to another term or an attempt to obtain a bit or qubit from a
-- collection of registers
data Expression = Var Id  | RegisterAccess{registerName:: Id,  registerNumber::Idx} deriving (Show, Eq)

-- This data type represents the application of gates to qubits.
data GateApp = H Expression | T Expression | Tdg Expression | ControlledNot Expression Expression deriving (Show, Eq)

type NatNum = WithContext NonNeg LineNumber

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

data TermType
  = Bit
  | Qbit
  | RegisterGroup RegisterType NatNum
  | Unit
  deriving (Show, Eq)

-- This data type represents evaluating gate applications under a context
-- where a quantum register collection is available.
data Command = QRegDeclIn{regCollName :: Identifier, numOfRegs :: NatNum, innerExpr :: Command}
  | Gate GateApp
   deriving (Show, Eq)
