{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Syntax(Expression(..),
          WithContext(..),
          Identifier,
          Index(..),
          Idx,
          GateInfo(..),
          IndexCoeffs,
          Id,
          GateApp(..),
          IndexVar(..),
          TermType(..),
          RegCollInfo(..),
          RegisterType(..),
          Command(..),
          toConstIdx,
          GateArg(..),
          idxVarsCoefficients,
          constPortion) where

import Lexer(LineNumber)
import Data.Function(on, (&))
import Control.Arrow((***))
import Control.Monad(join)
import Data.Ix(Ix, range, inRange)
import qualified Data.Map as M
import qualified Control.Lens as L
import GHC.Generics (Generic)
type Identifier = String

-- This data type represents a value along with its associated
-- context, e.g., where the file was found, the type of the value, etc.
data WithContext a ctx = WithContext a ctx deriving (Eq, Show)

instance (Ord a, Ord b) => Ord (WithContext a b) where
  (WithContext x _) <= (WithContext y _) = x <= y

type Id = WithContext Identifier LineNumber

newtype IndexVar = IndexVar String deriving (Eq, Show, Ord, Generic)

type IndexCoeffs = M.Map IndexVar Int

-- This data type represents values used to access and declare
-- register collections, being  a linear combination of
-- constants and index variables
data Index = Index{_constPortion :: Int, _idxVarsCoefficients :: IndexCoeffs} deriving (Show, Eq)
L.makeLenses ''Index

instance Ord Index where
  (<=) = (<=) `on` L.view constPortion

toConstIdx :: Int -> Index
toConstIdx val = Index val M.empty

tupleMap :: (a -> b) -> (a, a) -> (b, b)
tupleMap = join (***)


-- Takes two linear combinations of index variables, a, b,
-- and returns the difference of a and b
difference :: IndexCoeffs -> IndexCoeffs -> IndexCoeffs
difference a b = M.union negVarsOnlyInB varsInAOrB & M.filter (/= 0)
  where
    negVarsOnlyInB = M.difference b a  & M.map negate
    varsInAOrB = M.unionWith (-) a b

sumCoeffs :: IndexCoeffs -> IndexCoeffs -> IndexCoeffs
sumCoeffs = M.unionWith (+)

-- Takes two binary operations, one which operates on the constant portions of two indices, the other which operates on
-- the parametric components, two indices, and generates a new index with the results of applying the
-- functions to the corresponding components of each index
applyBinOpOnIndices :: (Int -> Int -> Int) -> (IndexCoeffs -> IndexCoeffs -> IndexCoeffs) -> Index -> Index -> Index
applyBinOpOnIndices f g (Index left indexVarsL) (Index right indexVarsR) = Index (f left right) $ g indexVarsL indexVarsR

instance Ix Index where
  range _ = error "Did not implement this yet"
  inRange idxBound x = inRange (tupleMap (L.view constPortion) idxBound) (L.view constPortion x)

instance Num Index where
  (+) = applyBinOpOnIndices (+) sumCoeffs
  (-) = applyBinOpOnIndices (-) difference
  (Index left _) * (Index right _) = Index (left * right) $ M.empty
  abs = error "No need to implement this"
  signum = error "No need to implement this"
  fromInteger = error "No need to implement this"
  negate = error "No need to implement this"


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
  | GateDecl GateInfo -- Declare an unscoped  gate
  | ConditionalGateExec{bitToTest :: Expression, toBeExecuted :: GateApp} -- Execute a gate if the given bit has a specific value
  | GateFamilyDecl{indexVars :: [IndexVar], gate :: GateInfo} -- Declare an unscoped parameterized gate
   deriving (Show, Eq)
