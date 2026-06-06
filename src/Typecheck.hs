{-# LANGUAGE NamedFieldPuns #-}

module Typecheck
    (determineType,
      TermType(..),
      TypeEvaluationError(..),
    ) where

import qualified Data.Map as M
import Control.Arrow ((>>>))
import Syntax(Identifier,
              Expression(..))



-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType

-- This data type represents that a register can contain either a classical or a quantum bit
data RegisterType = Quantum | Classical deriving (Show, Eq)

newtype Pos = Pos Int deriving (Show, Eq, Ord)

data TermType
  = Bit
  | Qbit
  | RegisterGroup RegisterType Pos
  deriving (Show, Eq)

-- This data type represents all the possible reasons for why the type of an expression cannot be
-- determined
data TypeEvaluationError = VariableNotInScope Identifier deriving (Show, Eq)

-- This type represents the result of determining the type of an
-- expression, being either a valid type or one that is invalid due to one or more reasons.
type TypeCalculationResult = Either TypeEvaluationError TermType

-- Takes an id referring to an expression, an evaluation scope, and returns the type of the referenced
-- expression if it exists. Returns an error otherwise.
findTypeWithinScope :: Identifier -> EvaluationContext -> TypeCalculationResult

findTypeWithinScope varName = M.lookup varName >>> maybe (Left $ VariableNotInScope varName) Right

-- Takes a predicate, a function to generate an err, the input, and
-- returns an error if the data does not satisfy the predicate. Returns the
-- data otherwise.
eitherFromPred :: (a -> Bool) -> (a -> err) -> a -> Either err a

eitherFromPred predicate elseCase x = if predicate x then Right x else (Left . elseCase) x


-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Expression -> TypeCalculationResult
determineType m (RegisterAccess{registerName, registerNumber}) =
  error "Need to implement the register access case"
--  findTypeWithinScope registerName m
--  >>= eitherFromPred isAccessingRegColl genMismatchInfo
--  >>= eitherFromPred (isAccessingValidReg registerNumber) (const UsesInvalidArrayIndex)
--  & fmap getRegisterContentType
--  where
--    isAccessingValidReg :: Index -> TermType -> Bool
--    isAccessingValidReg (Nat registerIdx) (RegisterGroup _ (Pos regCount)) = regCount > registerIdx
--    isAccessingValidReg _ _ = False
--
--    getRegisterContentType :: TermType -> TermType
--    getRegisterContentType (RegisterGroup Quantum _ ) = Qbit
--    getRegisterContentType (RegisterGroup Classical _ ) = Bit
--    getRegisterContentType _ = error "Should only have received a collection of registers"
--
--    isAccessingRegColl :: TermType -> Bool
--    isAccessingRegColl (RegisterGroup _ _)  = True
--    isAccessingRegColl _  = False
--
--    genMinRegCollSize (Nat v) = Pos $ v + 1
--    genMismatchInfo actType =  TypeMismatch registerName actType $ ExpectedAtLeastNRegs $ genMinRegCollSize registerNumber


determineType _ _ = error "Have not implemented this yet"
