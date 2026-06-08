module Syntax(Expression(..),
          WithContext(..),
          Identifier,
          Index(..),
          Idx,
          Id,
          PosNum(..),
          GateApp(..),
          Command(..)) where

import Lexer(LineNumber)

type Identifier = String

-- This data type represents a value along with its associated
-- context, e.g., where the file was found, the type of the value, etc.
data WithContext a ctx = WithContext a ctx deriving (Eq, Show)

type Id = WithContext Identifier LineNumber

newtype Index = Index Int deriving (Eq, Show)

type Idx = WithContext Index LineNumber

-- This data type represents the values an expression can take on,
-- being either a reference to another term or an attempt to obtain a bit or qubit from a
-- collection of registers
data Expression = Var Id  | RegisterAccess{registerName:: Id,  registerNumber::Idx} deriving (Show, Eq)

-- This data type represents the application of gates to qubits.
data GateApp = H Expression

-- Represents a positive number
newtype PosNum = PosNum Int deriving (Eq, Show)

-- This data type represents evaluating gate applications under a context
-- where a quantum register collection is available.
data Command = QRegDeclIn{regCollName :: Identifier, numOfRegs :: PosNum, innerExpr :: Command}  | Gate GateApp
