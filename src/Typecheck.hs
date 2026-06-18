{-# LANGUAGE GHC2024 #-}

module Typecheck
    (determineType,
      TypeEvaluationError(..),
      TypeErrAt,
      Term
    ) where

import qualified Data.Map as M
import Control.Arrow ((>>>))
import Syntax(Identifier,
              Expression(..),
              TermType(..),
              WithContext(..),
              Id,
              Index,
              GateArg(..),
              Idx,
              NonNeg(..),
              GateApp(..),
              RegisterType(..),
              Command(..))
import Lexer(LineNumber(..))
import Vary (Vary)
import qualified Vary
import Data.Function ((&))


-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType

-- This data type represents all the possible reasons for why the type of an expression cannot be
-- determined
data TypeEvaluationError = VariableNotInScope Identifier | EmptyRegCollDecl Identifier | InvalidRegAccess{collName :: Identifier, invalidIdx ::Index} deriving (Show, Eq)

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

verifyRegAccess m (RegisterAccess registerName@(WithContext name _) regIdx@(WithContext num lineNum)) =
  findTypeWithinScope registerName m
  & eitherFromPred (isAccessingValidReg regIdx) genInvalidAccessErr
  & (<$) Qbit
  where
    isAccessingValidReg :: Idx -> TermType -> Bool
    isAccessingValidReg (WithContext regIdx' _) (RegisterGroup Quantum (WithContext numOfRegs _)) = regIdx' < numOfRegs

    genInvalidAccessErr :: TermType -> TypeErrAt
    genInvalidAccessErr = const $ WithContext (InvalidRegAccess name num) lineNum

-- Takes the current context, the application of a gate, and
-- verifies if the application is valid under the given context.
-- Returns the type of the application if so. Returns an error otherwise.
verifyGateApp :: EvaluationContext -> GateApp -> TypeCalculationResult

verifyGateApp m (App gateName args) = do
  expectedArgs <- findGateType gateName m
  actualArgs <- traverse (verifyExpr m) args
  verifyGateArgs expectedArgs actualArgs
  where
    verifyGateArgs :: TermType -> [TermType] -> TypeCalculationResult
    verifyGateArgs (Circuit expectedArgs) actualArgs =  if expectedArgs == actualArgs then Right Unit else error "h"

    isCircuit :: TermType -> Bool
    isCircuit (Circuit _) = True
    isCircuit _ = False

    findGateType :: Id -> EvaluationContext -> TypeCalculationResult
    findGateType name  ctx = findTypeWithinScope name ctx & eitherFromPred isCircuit (error "Have not implemented this yet")

verifyExpr :: EvaluationContext -> Expression -> TypeCalculationResult

verifyExpr m x@(RegisterAccess _ _) = verifyRegAccess m x

verifyExpr m (Var varName) = findTypeWithinScope varName m


type Term = Vary '[Expression, GateApp, Command]

verifyCommand :: EvaluationContext -> Command -> TypeCalculationResult

-- Verifies that applying a gate produces a valid type.
verifyCommand m (Gate x@(App _ _)) = verifyGateApp m x

-- Verifies that declaring a gate and then applying it is valid
verifyCommand m (GateDecl{gateName, args, gateBody, innerExpr}) =
  verifyGateApp gateCtx gateBody *> verifyCommand commandCtx innerExpr
  where
    gateCtx = foldr extendCtxWithGateParam m args

    extendCtxWithGateParam :: GateArg -> EvaluationContext -> EvaluationContext
    extendCtxWithGateParam (GateArg{name, argType}) = M.insert name argType

    commandCtx = extendCtxWithCircuit gateName args m
    extendCtxWithCircuit circName circArgs = M.insert circName (genCircuit circArgs)
    genCircuit = Circuit . map argType

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



