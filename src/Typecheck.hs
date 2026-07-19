{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RecordWildCards #-}
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
              RegCollInfo(..),
              GateInfo(..),
              GateArg(..),
              Idx,
              NonNeg(..),
              GateApp(..),
              RegisterType(..),
              Command(..), RegCollInfo)
import Lexer(LineNumber(..))
import Vary (Vary)
import qualified Vary
import Data.Function ((&), on)
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

extractVal :: WithContext a b -> a
extractVal (WithContext x _) = x

verifyRegAccess m (RegisterAccess registerName@(WithContext name _) regIdx@(WithContext num lineNum)) =
  findTypeWithinScope registerName m
  & eitherFromPred (isAccessingValidReg regIdx) genInvalidAccessErr
  & fmap determineRegElemType
  where
    isAccessingValidReg :: Idx -> TermType -> Bool
    isAccessingValidReg regIdx' (RegisterGroup _ numOfRegs) = ((<) `on` extractVal) regIdx' numOfRegs

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

-- Takes the expected and actual argument types to a gate and
-- returns true if the expected types correspond to the actual types.
-- Returns false otherwise
isValidGateApp :: [TermType] -> [TermType] -> Bool
isValidGateApp expectedArgTypes  = zip expectedArgTypes >>> all (uncurry isSupertypeOf)
  where
    isSupertypeOf :: TermType -> TermType -> Bool
    isSupertypeOf (RegisterGroup collTy numOfRegs) (RegisterGroup collTy' numOfRegs') = collTy == collTy' && extractVal numOfRegs <= extractVal numOfRegs'
    isSupertypeOf x y = x == y

-- Takes the line where a gate was applied,
-- the types of the expected arguments for a gate,
-- the types of the actual arguments passed to the gate,
-- the arguments passed to the gate, and checks if the
-- expected and actual types match. Returns an error otherwise
verifyGateArgs :: LineNumber -> TermType -> [TermType] -> [Expression] -> TypeCalculationResult

verifyGateArgs line (Circuit expectedArgTypes) actualArgTypes args
  | gateIsAppliedToTooManyArgs = unexpectedNumOfArgsErr
  | gateIsAppliedToTooFewArgs = unexpectedNumOfArgsErr
  | isValidGateApp expectedArgTypes actualArgTypes  = Right Unit
  | otherwise = gateArgMismatchErr
  where
    numOfExpectedTypes = length expectedArgTypes
    numOfActualTypes = length actualArgTypes
    gateIsAppliedToTooManyArgs = numOfExpectedTypes < numOfActualTypes
    gateIsAppliedToTooFewArgs = numOfExpectedTypes > numOfActualTypes
    unexpectedNumOfArgsErr = Left $ WithContext ExpectedNParams{expectedNumOfParams = NonNeg numOfExpectedTypes, actualNumOfParams = NonNeg numOfActualTypes} line
    gateArgMismatchErr = Left $ WithContext (findTypeMismatch args expectedArgTypes actualArgTypes) line


-- Takes the current context, an expression, and calculates its type
-- under the given context
verifyExpr :: EvaluationContext -> Expression -> TypeCalculationResult
verifyExpr m x@(RegisterAccess{}) = verifyRegAccess m x

verifyExpr m (Var varName) = findTypeWithinScope varName m

-- Takes the current context, the application of a gate, and
-- verifies if the application is valid under the given context.
-- Returns the type of the application if so. Returns an error otherwise.
verifyGateApp :: EvaluationContext -> GateApp -> TypeCalculationResult

verifyGateApp m (GateApp gateName@(WithContext _ line) args) = do
  expectedTypes <- findGateType gateName m
  actualTypes <- traverse (verifyExpr m) args
  verifyGateArgs line expectedTypes actualTypes args
  where
    isCircuit :: TermType -> Bool
    isCircuit (Circuit _) = True
    isCircuit _ = False

    findGateType :: Id -> EvaluationContext -> TypeCalculationResult
    findGateType name  = findTypeWithinScope name  >>> eitherFromPred isCircuit genIsNotGateErr
    genIsNotGateErr :: TermType -> TypeErrAt
    genIsNotGateErr = flip ExpectedAGate gateName  >>> flip WithContext line

type Term = Vary '[Expression, GateApp, Command]

verifyCommand :: EvaluationContext -> Command -> TypeCalculationResult

-- Verifies that applying a gate produces a valid type.
verifyCommand m (Gate x@(GateApp{})) = verifyGateApp m x

-- Verifies that declaring a scoped gate and then applying it is valid
verifyCommand m ScopedGateDecl{..} = verifyOnlyIfGateDeclIsValid info m innerExpr

verifyCommand m (Sequence (GateDecl info) y) = verifyOnlyIfGateDeclIsValid info m y
verifyCommand m (GateDecl info) = verifyGateDecl info m

-- Checks that a non-empty register collection is being declared and used
-- validly in the inner expression
verifyCommand m ScopedRegCollDecl{..} = evalIfRegCollDeclIsValid m coll innerExpr

-- Verifies that a qubit is being measured and stored in a bit
verifyCommand m (QubitMeasurement toMeasure toStoreIn) =
  verifyMeasuredQubit *> verifyStoredBit $> Unit
  where
    verifyMeasuredQubit = verifyExprType m Qbit toMeasure
    verifyStoredBit = verifyExprType m Bit toStoreIn

verifyCommand m (Sequence (RegCollDecl collInfo) y) = evalIfRegCollDeclIsValid m collInfo y

verifyCommand _ (RegCollDecl info)
  | isEmptyRegColl info = genEmptyRegCollDeclErr info
  | otherwise = Right Unit

verifyCommand m (Sequence x y) = verifyCommand m x *> verifyCommand m y

verifyCommand m (QubitReset potentialQubit) = verifyExprType m Qbit potentialQubit $> Unit

verifyCommand m ConditionalGateExec{bitToTest, toBeExecuted} = verifyExprType m Bit bitToTest *> verifyGateApp m toBeExecuted

-- Takes information about a gate declaration, the local context, and
-- checks that the body of the gate is valid according to the
-- parameters in the declaration and the context. Returns an error otherwise
verifyGateDecl :: GateInfo -> EvaluationContext -> TypeCalculationResult
verifyGateDecl GateInfo{..} m = gateDeclCtx >>= (`verifyGateApp`  gateBody)
  where
    gateDeclCtx = foldr extendCtxWithGateParam m <$> traverse verifyTypeAnnotation args
    extendCtxWithGateParam :: GateArg -> EvaluationContext -> EvaluationContext
    extendCtxWithGateParam (GateArg{..}) = M.insert name argType

    -- Checks that a type annotation is valid. Returns an error otherwise
    verifyTypeAnnotation :: GateArg -> Either TypeErrAt GateArg
    verifyTypeAnnotation arg@(GateArg regCollName (RegisterGroup collType numOfRegs))
      | NonNeg 0 == extractVal numOfRegs = genEmptyRegCollDeclErr  RegCollInfo {..}
      | otherwise = return arg

    verifyTypeAnnotation x  = return x


-- Takes information about a gate declaration, the context under which to evaluate the
-- declaration, a command, and evaluates the command with the gate type embedded in the context
-- if the declaration is valid. Returns an error otherwise
verifyOnlyIfGateDeclIsValid :: GateInfo -> EvaluationContext -> Command -> TypeCalculationResult
verifyOnlyIfGateDeclIsValid info@GateInfo{gateName, args} m toVerify =  verifyGateDecl info m  *> verifyCommand extendedCtx toVerify
  where
    extendedCtx = extendCtxWithCircuit gateName args m
    extendCtxWithCircuit circName circArgs = M.insert circName (genCircuit circArgs)
    genCircuit = Circuit . map argType

-- Takes the expected type of an expression, an expression, the actual type of the expression,
--  and generates an error noting that the actual and expected types do not match
genMismatchErr :: TermType -> Expression -> TermType -> TypeErrAt
genMismatchErr expectedType erroneousTerm actualType = WithContext TypeMismatch{..} (getLineNum erroneousTerm)
  where
    -- Takes an expression and returns the line at where the
    -- expression was found
    getLineNum :: Expression -> LineNumber
    getLineNum (Var varName) = extractCtx varName
    getLineNum RegisterAccess{registerName} = extractCtx registerName

-- Takes the context under which to evaluate an expression, the expected type of the
-- expression, an expression, and returns the actual type of the expression if it matches
-- the expected one. Returns an error otherwise.
verifyExprType :: EvaluationContext -> TermType -> Expression -> TypeCalculationResult

verifyExprType m expectedType toVerify = verifyExpr m toVerify & eitherFromPred (== expectedType) (genMismatchErr expectedType toVerify)

-- Takes the current context, the makeup of a register collection
-- declaration, a command to evaluate, and evaluates the command under
-- the context updated with the declaration if an empty collection is not
-- being declared. Returns an error otherwise
evalIfRegCollDeclIsValid :: EvaluationContext -> RegCollInfo -> Command -> TypeCalculationResult
evalIfRegCollDeclIsValid ctx declInfo toEval
  | isEmptyRegColl declInfo = genEmptyRegCollDeclErr declInfo
  | otherwise = verifyCommand newContext toEval
  where
    newContext = addRegCollToCtx declInfo ctx

extractCtx :: WithContext a b -> b
extractCtx (WithContext _ x) = x

isEmptyRegColl :: RegCollInfo -> Bool
isEmptyRegColl = getRegCount >>> (== NonNeg 0)
  where
    getRegCount =  numOfRegs >>> extractVal

genEmptyRegCollDeclErr :: RegCollInfo -> Either TypeErrAt a
genEmptyRegCollDeclErr RegCollInfo{..} = Left $ WithContext (EmptyRegCollDecl regCollName) (extractCtx numOfRegs)

-- Takes the name and kind of a register collection along with the number of registers
-- and updates the current evaluation context with the type of the collection
addRegCollToCtx :: RegCollInfo -> EvaluationContext -> EvaluationContext

addRegCollToCtx RegCollInfo{..} = M.insert regCollName (RegisterGroup collType numOfRegs)

-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Term -> TypeCalculationResult

determineType m term = term &
  (Vary.on @Expression (verifyExpr m)
  $ Vary.on @GateApp (verifyGateApp m)
  $ Vary.on @Command (verifyCommand m)
   $ Vary.exhaustiveCase  )
