module Syntax(Expression(..),
          WithContext(..),
          Identifier,
          Index(..),
          Idx,
          GateInfo(..),
          Id,
          GateApp(..),
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

data Index =
  Const NonNeg
  | Sum Index Index
  deriving (Eq, Show)


type Idx = WithContext Index LineNumber

-- This data type represents the values an expression can take on,
-- being either a reference to another term or an attempt to obtain a bit or qubit from a
-- collection of registers
data Expression = Var Id  | RegisterAccess{registerName:: Id,  registerNumber::Idx} deriving (Show, Eq)

-- This data type represents that the application of a gate can consist of
-- a single gate or the combination of two or more gates, where the left most gate
-- is evaluated first
data GateApp =
  GateApp{gateId :: Id, gateArgs :: [Expression]}
  | GateSequence GateApp GateApp
  deriving (Show, Eq)

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

data TermType
  = Bit
  | Qbit
  | RegisterGroup RegisterType Idx
  | Unit
  | Circuit{circuitArgs :: [TermType]}
  deriving (Show)

instance Eq TermType where
  Bit == Bit = True
  Qbit == Qbit = True
  Unit == Unit = True
  RegisterGroup x (WithContext v _) == RegisterGroup y (WithContext w _) = x == y && v == w
  (Circuit args') == (Circuit args'') = args' == args''
  _ == _ = False

data GateArg = GateArg{name :: Identifier, argType :: TermType} deriving (Show, Eq)

-- This data type represents the information that characterizes a register collection, being its name,
-- the kind of elements present, and the number of registers
data RegCollInfo = RegCollInfo{collType :: RegisterType, regCollName :: Identifier, numOfRegs :: Idx} deriving (Eq, Show)

-- This type represents information known about a gate, namely its name, the arguments it takes,
-- and the body of the gate
data GateInfo = GateInfo{gateName :: Identifier, args :: [GateArg], gateBody :: GateApp} deriving (Show, Eq)

-- This data type represents all possible commands a user can execute.
data Command = Gate GateApp -- Apply a gate to one or more qubits
  | ScopedGateDecl {info :: GateInfo, innerExpr :: Command} -- Declare a gate and use it in a later expression
  | ScopedRegCollDecl {coll :: RegCollInfo, innerExpr :: Command} -- Declare a register collection and use it in a later expression
  | RegCollDecl RegCollInfo -- Declare a register collection
  | Sequence Command Command -- Evaluates the second command under the context obtained from evaluating the first
  | QubitMeasurement{toMeasure :: Expression, toStoreIn :: Expression} -- Measure a qubit and store the measurement in a bit
  | QubitReset{toReset :: Expression}
  | GateDecl GateInfo
  | ConditionalGateExec{bitToTest :: Expression, toBeExecuted :: GateApp}
   deriving (Show, Eq)
