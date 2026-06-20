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
          Command(..),
          GateArg(..)) where

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
data GateApp = App{gateId :: Id, gateArgs :: [Expression]} deriving (Show, Eq)

type NatNum = WithContext NonNeg LineNumber

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

data TermType
  = Bit
  | Qbit
  | RegisterGroup RegisterType NatNum
  | Unit
  | Circuit{circuitArgs :: [TermType]}
  deriving (Show, Eq)

data GateArg = GateArg{name :: Identifier, argType :: TermType} deriving (Show, Eq)

-- This data type represents all possible commands a user can execute.
data Command = Gate GateApp -- Apply a gate to one or more qubits
  | DeclGateIn {gateName :: Identifier, args :: [GateArg], gateBody :: GateApp, innerExpr :: Command} -- Declare a gate and use it in a later expression
  | DeclRegCollIn {collType :: RegisterType, regCollName :: Identifier, numOfRegs :: NatNum, innerExpr :: Command} -- Declare a register collection and use it in a later expression
  | MeasureQubit{toMeasure :: Expression, toStoreIn :: Expression} -- Measure a qubit and store the measurement in a bit
   deriving (Show, Eq)
