{-# LANGUAGE GHC2024 #-}

module Typecheck
    (determineType,
      TermType(..),
      TypeEvaluationError(..),
      TypeErrAt,
      Term
    ) where

import qualified Data.Map as M
import Control.Arrow ((>>>))
import Syntax(Identifier,
              Expression(..),
              WithContext(..),
              Id,
              Idx,
              Index(..),
              NonNeg(..),
              GateApp(..),
              Command(..),
              NatNum)
import Lexer(LineNumber(..))
import Vary (Vary)
import qualified Vary
import Data.Function ((&))


-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

data TermType
  = Bit
  | Qbit
  | RegisterGroup RegisterType NatNum
  | Unit
  deriving (Show, Eq)

-- This data type represents all the possible reasons for why the type of an expression cannot be
-- determined
data TypeEvaluationError = VariableNotInScope Identifier | EmptyRegCollDecl Identifier deriving (Show, Eq)

type TypeErrAt = WithContext TypeEvaluationError LineNumber

-- This type represents the result of determining the type of an
-- expression, being either a valid type or one that is invalid due to one or more reasons.
type TypeCalculationResult = Either TypeErrAt TermType

-- Takes an id referring to an expression, an evaluation scope, and returns the type of the referenced
-- expression if it exists. Returns an error otherwise.
findTypeWithinScope :: Id -> EvaluationContext -> TypeCalculationResult

findTypeWithinScope (WithContext varName lineNum) = M.lookup varName >>> maybe lookupErr Right
  where
    lookupErr = Left $ WithContext (VariableNotInScope varName) lineNum

eitherFromPred :: (a -> Bool) -> (a -> err) -> Either err a -> Either err a
eitherFromPred predicate errFn = (>>= \x -> if predicate x then return x else Left (errFn x))

-- Takes the current context, an request to access a register collection, and
-- verifies if the request is valid, i.e., if the register collection exists and
-- a valid register is selected. Returns the type of the register if so or an
-- error otherwise
verifyRegAccess :: EvaluationContext -> Expression -> TypeCalculationResult

verifyRegAccess m (RegisterAccess registerName regIdx) =
  findTypeWithinScope registerName m
  & eitherFromPred (isAccessingValidReg regIdx) (error "Have not handled the case where the register access is invalid")
  & fmap (const Qbit)
  where
    isAccessingValidReg :: Idx -> TermType -> Bool
    isAccessingValidReg (WithContext (Index regIdx) _) (RegisterGroup Quantum (WithContext (NonNeg numOfRegs) _)) = regIdx < numOfRegs

isQubit :: TermType -> Bool

isQubit = (== Qbit)

-- Takes the current context, the application of a gate, and
-- verifies if the application is valid under the given context.
-- Returns the type of the application if so. Returns an error otherwise.
verifyGateApp :: EvaluationContext -> GateApp -> TypeCalculationResult

verifyGateApp m (H regColl@(RegisterAccess _ _)) =
  verifyRegAccess m regColl
  & eitherFromPred isQubit (error "Have not handled the case where the register access is invalid")
  & fmap (const Unit)

verifyGateApp m (H (Var varName)) =
  findTypeWithinScope varName m
  & eitherFromPred isQubit (error "Have not handled the case where the variable is not a qubit")

type Term = Vary '[Expression, GateApp, Command]

-- Verifies that executing a command produces a valid type.
verifyCommand :: EvaluationContext -> Command -> TypeCalculationResult
verifyCommand m (Gate x@(H _)) = verifyGateApp m x

verifyCommand m (QRegDeclIn regCollName numOfRegs@(WithContext num lineNum) innerExpr)
  | isEmptyRegColl  = emptyRegCollDeclErr
  | otherwise = verifyCommand newContext innerExpr
  where
    newContext = M.insert regCollName (RegisterGroup Quantum numOfRegs) m
    isEmptyRegColl = num == NonNeg 0
    emptyRegCollDeclErr = Left $ WithContext (EmptyRegCollDecl regCollName) lineNum


-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Term -> TypeCalculationResult

determineType m term = term &
  (Vary.on @Expression (verifyRegAccess m)
  $ Vary.on @GateApp (verifyGateApp m)
  $ Vary.on @Command (verifyCommand m)
   $ Vary.exhaustiveCase  )



