module Syntax(Expression(..),
          WithContext(..),
          Identifier,
          Index,
          Idx,
          Id,
          GateApp(..),
          NatNum,
          TermType(..),
          RegCollInfo(..),
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
data GateApp = GateApp{gateId :: Id, gateArgs :: [Expression]} deriving (Show, Eq)

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

-- This data type represents the information that characterizes a register collection, being its name,
-- the kind of elements present, and the number of registers
data RegCollInfo = RegCollInfo{collType :: RegisterType, regCollName :: Identifier, numOfRegs :: NatNum} deriving (Eq, Show)

-- This data type represents all possible commands a user can execute.
data Command = Gate GateApp -- Apply a gate to one or more qubits
  | ScopedGateDecl {gateName :: Identifier, args :: [GateArg], gateBody :: GateApp, innerExpr :: Command} -- Declare a gate and use it in a later expression
  | ScopedRegCollDecl {coll :: RegCollInfo, innerExpr :: Command} -- Declare a register collection and use it in a later expression
  | RegCollDecl RegCollInfo -- Declare a register collection
  | Sequence Command Command -- Evaluates the second command under the context obtained from evaluating the first
  | QubitMeasurement{toMeasure :: Expression, toStoreIn :: Expression} -- Measure a qubit and store the measurement in a bit
   deriving (Show, Eq)
