{-# LANGUAGE GHC2024 #-}
module Typecheck
    (determineType,
      TypeEvaluationError(..),
      TypeErrAt,
      Term)
where

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
import Data.Functor(($>))
import Data.List(findIndex)
import Data.Maybe(fromJust)

-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType

-- This data type represents all the possible reasons for why the type of an expression cannot be
-- determined
data TypeEvaluationError = VariableNotInScope Identifier
  | EmptyRegCollDecl Identifier
  | InvalidRegAccess{collName :: Identifier, invalidIdx ::Index}
  | ExpectedNParams{expectedNumOfParams :: NonNeg, actualNumOfParams :: NonNeg}
  | TypeMismatch{expectedType :: TermType, actualType :: TermType, erroneousTerm :: Expression}
  | ExpectedAGate{actualType :: TermType, problemTerm :: Id}
  deriving (Show, Eq)

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
  & fmap determineRegElemType
  where
    isAccessingValidReg :: Idx -> TermType -> Bool
    isAccessingValidReg (WithContext regIdx' _) (RegisterGroup _ (WithContext numOfRegs _)) = regIdx' < numOfRegs

    determineRegElemType :: TermType -> TermType
    determineRegElemType (RegisterGroup Quantum _) = Qbit
    determineRegElemType (RegisterGroup Classical _) = Bit

    genInvalidAccessErr :: TermType -> TypeErrAt
    genInvalidAccessErr = const $ WithContext (InvalidRegAccess name num) lineNum

-- Takes two lists of the same length where they differ elementwise and
-- returns the index of the first elementwise difference between both lists
findIdxOfFirstDiff :: Eq a => [a] -> [a] -> Int
findIdxOfFirstDiff x = zipWith (/=) x >>> findIndex id >>> fromJust

-- Takes a collection of arguments passed to a gate
-- where one of them does not have the expected type,
-- the expected and actual types of the arguments to the
-- gate, and generates an error noting that the aforementioned
-- argument has the wrong type
findTypeMismatch :: [Expression] -> [TermType] -> [TermType] -> TypeEvaluationError

findTypeMismatch actualArgs expectedArgTypes actualArgTypes =
  TypeMismatch{expectedType, actualType, erroneousTerm}
  where
    mismatchIdx = findIdxOfFirstDiff actualArgTypes expectedArgTypes
    [expectedType, actualType] = map (!! mismatchIdx) [expectedArgTypes, actualArgTypes]
    erroneousTerm = actualArgs !! mismatchIdx

-- Takes the line where a gate was applied,
-- the types of the expected arguments for a gate,
-- the types of the actual arguments passed to the gate,
-- the arguments passed to the gate, and checks if the
-- expected and actual types match. Returns an error otherwise
verifyGateArgs :: LineNumber -> TermType -> [TermType] -> [Expression] -> TypeCalculationResult

verifyGateArgs line (Circuit expectedArgTypes) actualArgTypes args
  | gateIsAppliedToTooManyArgs = unexpectedNumOfArgsErr
  | gateIsAppliedToTooFewArgs = unexpectedNumOfArgsErr
  | expectedArgTypes == actualArgTypes  = Right Unit
  | otherwise = gateArgMismatchErr
  where
    numOfExpectedTypes = length expectedArgTypes
    numOfActualTypes = length actualArgTypes
    gateIsAppliedToTooManyArgs = numOfExpectedTypes < numOfActualTypes
    gateIsAppliedToTooFewArgs = numOfExpectedTypes > numOfActualTypes
    unexpectedNumOfArgsErr = Left $ WithContext ExpectedNParams{expectedNumOfParams = NonNeg numOfExpectedTypes, actualNumOfParams = NonNeg numOfActualTypes} line
    gateArgMismatchErr = Left $ WithContext (findTypeMismatch args expectedArgTypes actualArgTypes) line


-- Takes the current context, the application of a gate, and
-- verifies if the application is valid under the given context.
-- Returns the type of the application if so. Returns an error otherwise.
verifyGateApp :: EvaluationContext -> GateApp -> TypeCalculationResult

verifyGateApp m (App gateName@(WithContext _ line) args) = do
  expectedTypes <- findGateType gateName m
  actualTypes <- traverse (verifyExpr m) args
  verifyGateArgs line expectedTypes actualTypes args
  where
    isCircuit :: TermType -> Bool
    isCircuit (Circuit _) = True
    isCircuit _ = False

    findGateType :: Id -> EvaluationContext -> TypeCalculationResult
    findGateType name  = findTypeWithinScope name  >>> eitherFromPred isCircuit genMismatchErr
    genMismatchErr = flip ExpectedAGate gateName  >>> flip WithContext line 

-- Takes the current context, an expression, and calculates its type
-- under the given context
verifyExpr :: EvaluationContext -> Expression -> TypeCalculationResult
verifyExpr m x@(RegisterAccess _ _) = verifyRegAccess m x

verifyExpr m (Var varName) = findTypeWithinScope varName m


type Term = Vary '[Expression, GateApp, Command]

verifyCommand :: EvaluationContext -> Command -> TypeCalculationResult

-- Verifies that applying a gate produces a valid type.
verifyCommand m (Gate x@(App _ _)) = verifyGateApp m x

-- Verifies that declaring a gate and then applying it is valid
verifyCommand m (DeclGateIn{gateName, args, gateBody, innerExpr}) =
  verifyGateApp gateCtx gateBody *> verifyCommand commandCtx innerExpr
  where
    gateCtx = foldr extendCtxWithGateParam m args

    extendCtxWithGateParam :: GateArg -> EvaluationContext -> EvaluationContext
    extendCtxWithGateParam (GateArg{name, argType}) = M.insert name argType

    commandCtx = extendCtxWithCircuit gateName args m
    extendCtxWithCircuit circName circArgs = M.insert circName (genCircuit circArgs)
    genCircuit = Circuit . map argType

-- Checks that a non-empty register collection is being declared and used
-- validly in the inner expression
verifyCommand m (DeclRegCollIn collType regCollName numOfRegs@(WithContext num lineNum) innerExpr)
  | isEmptyRegColl  = emptyRegCollDeclErr
  | otherwise = verifyCommand newContext innerExpr
  where
    newContext = M.insert regCollName (RegisterGroup collType numOfRegs) m
    isEmptyRegColl = num == NonNeg 0
    emptyRegCollDeclErr = Left $ WithContext (EmptyRegCollDecl regCollName) lineNum

-- Verifies that a qubit is being measured and stored in a bit
verifyCommand m (MeasureQubit toMeasure toStoreIn) =
  verifyMeasuredQubit *> verifyStoredBit $> Unit
  where
    verifyMeasuredQubit = verifyExpr m toMeasure & eitherFromPred (== Qbit) (error "Handle the case where the measured expression is not a qubit")
    verifyStoredBit = verifyExpr m toStoreIn & eitherFromPred (== Bit) (error "Handle the case where the expression to store the measured value in is not a bit")

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



