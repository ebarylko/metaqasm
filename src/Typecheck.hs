{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Typecheck
    (determineType,
      TypeEvaluationError(..),
      CounterExample(..),
      TypeErrAt,
      fromEither,
      Term)
where

import Data.Bool(bool)
import qualified Data.Map as M
import Control.Arrow ((>>>), (&&&))
import Syntax(Identifier,
              Expression(..),
              TermType(..),
              WithContext(..),
              Id,
              Index(..),
              IndexVar(..),
              RegCollInfo(..),
              idxVarsCoefficients,
              constPortion,
              toConstIdx,
              GateInfo(..),
              GateArg(..),
              Idx,
              GateApp(..),
              RegisterType(..),
              Command(..),
              RegCollInfo)
import Lexer(LineNumber(..))
import Vary (Vary)
import qualified Vary
import Data.Function ((&), on)
import Data.Functor(($>))
import Data.List(findIndex)
import Data.Maybe(fromJust)
import qualified Control.Lens as L hiding (Control.Lens.Index)
import Data.Ix(inRange)
import qualified Grisette as G
import Data.String(fromString)
import Control.Monad.Except(ExceptT(..))
import Data.Generics.Product (position)
import Control.Monad.Extra(allM)

-- This data type represents the context under which to evaluate
-- the type of a term
type EvaluationContext = M.Map Identifier TermType

-- This data type represents pairs of numbers which are supposed to
-- satisfy certain conditions and an example of how that condition
-- is not satisfied in a specific case
data CounterExample = CounterExample{toBeRefuted :: Index, example :: M.Map IndexVar Int} deriving (Show, Eq)

-- This data type represents all the possible reasons for why the type of an expression cannot be
-- determined
data TypeEvaluationError =
  VariableNotInScope Identifier
  | EmptyRegCollDecl Identifier
  | NegSizeRegCollDecl Identifier
  | UsesFreeIndexVar{invalidIdx :: Index, freeIdxVar :: IndexVar}
  | InvalidParametricRegCollDecl Identifier CounterExample
  | InvalidRegAccess{collName :: Identifier, invalidIdx ::Index}
  | ExpectedNParams{expectedNumOfParams :: Index, actualNumOfParams :: Index}
  | TypeMismatch{expectedType :: TermType, actualType :: TermType, erroneousTerm :: Expression}
  | ExpectedAGate{actualType :: TermType, problemTerm :: Id}
  | ExpectedARegColl{actualType :: TermType, notARegColl :: Expression}
  | InvalidCircuitAnnotation{invalidPart :: TermType}
  deriving (Show, Eq)


type TypeErrAt = WithContext TypeEvaluationError LineNumber

-- This type represents the result of determining the type of an
-- expression, being either a valid type or one that is invalid due to one or more reasons.
type TypeCalculationResult = Either TypeErrAt TermType

toTypeErrAtLoc :: TypeEvaluationError -> LineNumber -> Either TypeErrAt a
toTypeErrAtLoc err = Left . WithContext err

-- Takes an id referring to an expression, an evaluation scope, and returns the type of the referenced
-- expression if it exists. Returns an error otherwise.
findTypeWithinScope :: Id -> EvaluationContext -> TypeCalculationResult

findTypeWithinScope (WithContext varName lineNum) = M.lookup varName >>> maybe lookupErr Right
  where
    lookupErr = toTypeErrAtLoc (VariableNotInScope varName) lineNum

eitherFromPred :: (a -> Bool) -> (a -> err) -> Either err a -> Either err a
eitherFromPred predicate errFn = (>>= \x -> if predicate x then return x else Left (errFn x))


extractVal :: WithContext a b -> a
extractVal (WithContext x _) = x

L.makePrisms ''TermType

-- Takes the index used to access a register collection, information about said collection,
-- and returns true if the index lies within the bounds of the collection.
-- Returns false otherwise
isAccessingValidReg :: Idx -> TermType -> Bool
isAccessingValidReg regIdx' (RegisterGroup _ numOfRegs) =  (isIdxWithinArrayBounds `on` extractVal) regIdx' numOfRegs
  where
    isIdxWithinArrayBounds :: Index -> Index -> Bool
    isIdxWithinArrayBounds idx collBound = idx `inRange'` (zero, collBound - one)
    inRange' = flip inRange
    one = Index 1 M.empty

-- Takes the current context, an request to access a register collection, and
-- verifies if the request is valid, i.e., if the register collection exists and
-- a valid register is selected. Returns the type of the register if so or an
-- error otherwise
verifyRegAccess :: EvaluationContext -> Expression -> TypeCalculationResult

verifyRegAccess m (RegisterAccess registerName@(WithContext name _) regIdx@(WithContext num lineNum)) =
  findTypeWithinScope registerName m
  & eitherFromPred isAccessingRegColl genExpectedRegCollErr
  & eitherFromPred (isAccessingValidReg regIdx) genInvalidAccessErr
  & fmap determineRegElemType
  where
    isAccessingRegColl :: TermType -> Bool
    isAccessingRegColl = L.has _RegisterGroup

    genExpectedRegCollErr :: TermType -> TypeErrAt
    genExpectedRegCollErr = flip WithContext lineNum . flip ExpectedARegColl (Var registerName)

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
    isSupertypeOf (RegisterGroup collTy expectedNumOfRegs) (RegisterGroup collTy' actualNumOfRegs) = collTy == collTy' &&  expectedNumOfRegs <= actualNumOfRegs
    isSupertypeOf (Circuit left) (Circuit right) = all id (zipWith isSupertypeOf right left)
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
    unexpectedNumOfArgsErr = toTypeErrAtLoc ExpectedNParams{expectedNumOfParams = toConstIdx numOfExpectedTypes, actualNumOfParams = toConstIdx numOfActualTypes} line
    gateArgMismatchErr = toTypeErrAtLoc (findTypeMismatch args expectedArgTypes actualArgTypes) line


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

verifyGateApp m (GateSequence a b)
  = verifyGateApp m a *> verifyGateApp m b

verifyGateApp' :: EvaluationContext -> GateApp -> TypeCalculationResult'
verifyGateApp' m  = verifyGateApp m >>> fromEither

fromEither :: Monad m => Either err a -> ExceptT err m a
fromEither = return >>> ExceptT

type Term = Vary '[Expression, GateApp, Command]

verifyCommand :: EvaluationContext -> Command -> TypeCalculationResult'

-- Verifies that applying a gate produces a valid type.
verifyCommand m (Gate x@(GateApp{})) = verifyGateApp' m x

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
    verifyMeasuredQubit = verifyExprType' m Qbit toMeasure
    verifyStoredBit = verifyExprType' m Bit toStoreIn

verifyCommand m (Sequence (RegCollDecl collInfo) y) = evalIfRegCollDeclIsValid m collInfo y

verifyCommand _ (RegCollDecl info)  = doNothingIfRegCollDeclIsValid info
  where
    doNothingIfRegCollDeclIsValid :: RegCollInfo -> TypeCalculationResult'
    doNothingIfRegCollDeclIsValid  = applyFIfRegCollDeclIsValid  (const (Right Unit) >>> fromEither)

verifyCommand m (Sequence x y) = verifyCommand m x *> verifyCommand m y

verifyCommand m (QubitReset potentialQubit) = verifyExprType' m Qbit potentialQubit $> Unit

verifyCommand m ConditionalGateExec{bitToTest, toBeExecuted} = verifyExprType' m Bit bitToTest *> verifyGateApp' m toBeExecuted

verifyCommand m GateFamilyDecl{indexVars, gate} = verifyParametricGateDecl indexVars gate m

verifyExprType' :: EvaluationContext -> TermType -> Expression -> TypeCalculationResult'
verifyExprType' m expectedType = verifyExprType m expectedType >>> fromEither

-- Takes a number which is supposed to satisfy a property, an example of
-- how the number may not always satisfy the property, and yields a
-- counterexample about the validity of the number
genCounterExample :: Index ->  G.Model -> CounterExample
genCounterExample idx@(Index _ _idxVarsCoefficients) m = CounterExample idx counterExample
  where
    counterExample :: M.Map IndexVar Int
    counterExample = map (id &&& getVarVal)  (M.keys _idxVarsCoefficients) & M.fromList
    getVarVal :: IndexVar -> Int
    getVarVal = toSymVar >>> G.evalSymToCon m >>> fromInteger

-- Takes the number of elements in a parametric collection
-- declaration and  validates it if the collection is always
-- nonempty. Returns an error otherwise
proveCollIsNonEmpty :: Identifier -> Idx -> ExceptT TypeErrAt IO ()
proveCollIsNonEmpty collId (WithContext idx@(Index _constPortion _idxVarsCoefficients) line) = interpretProof collSizeProof
  where
    genInvalidLengthRegCollErr  :: G.Model -> Either TypeErrAt ()
    genInvalidLengthRegCollErr  = genCounterExample idx  >>> InvalidParametricRegCollDecl collId  >>> flip WithContext line  >>> Left
    collSizeProof :: IO (Either G.SolvingFailure G.Model)
    collSizeProof = G.solve G.z3 $ G.symNot $ givenIdxVarsAreNonNeg `G.symImplies` numOfRegsIsPos

    interpretProof :: IO (Either a G.Model) -> ExceptT TypeErrAt IO ()
    interpretProof = fmap convertProofToTyp >>> ExceptT
    convertProofToTyp = either (const $ Right ())  genInvalidLengthRegCollErr
    givenIdxVarsAreNonNeg :: G.SymBool
    givenIdxVarsAreNonNeg = M.keys _idxVarsCoefficients & foldr (genAndCombineConstraints . toSymVar) G.true
    genAndCombineConstraints :: G.SymInteger -> G.SymBool -> G.SymBool
    genAndCombineConstraints = (G..>= 0) >>> (G..&&)

    numOfRegsIsPos :: G.SymBool
    numOfRegsIsPos = toSymInt _constPortion + linearCombOfIdxVars G..> 0
    linearCombOfIdxVars :: G.SymInteger
    linearCombOfIdxVars = M.toList _idxVarsCoefficients
      & (L.each . L._1) L.%~ toSymVar
      & (L.each . L._2) L.%~ toSymInt
      & foldr (uncurry (*) >>> (+)) 0

-- Takes a number and returns the symbolic representation
-- of that number
toSymInt :: Int -> G.SymInteger
toSymInt = toInteger >>> G.con

-- Takes an index variable and converts it into a
-- symbolic variable
toSymVar :: IndexVar -> G.SymInteger
toSymVar = (L.^. position @1) >>>  fromString >>> G.ssym


-- Takes a collection of index variables that can be used, an argument to a gate, and
-- checks that the argument does not reference any free index variables
isNotUsingFreeIdxVar :: [IndexVar] -> GateArg -> ExceptT TypeErrAt IO Bool
isNotUsingFreeIdxVar validIdxVars (GateArg _ (RegisterGroup _ numOfRegs))  = allM (isValidIndexVar validIdxVars) usedIdxVars
  where
    usedIdxVars = extractVal numOfRegs & L.view idxVarsCoefficients & M.keys
    isValidIndexVar :: [IndexVar] -> IndexVar -> ExceptT TypeErrAt IO Bool
    isValidIndexVar inScopeIndexVars usedVar = bool  (fromEither $ genFreeIdxVarErr usedVar numOfRegs) (return True) $ usedVar `elem` inScopeIndexVars
    genFreeIdxVarErr :: IndexVar -> Idx -> Either TypeErrAt Bool
    genFreeIdxVarErr freeVar (WithContext regCount line) = toTypeErrAtLoc (UsesFreeIndexVar regCount freeVar) line

isNotUsingFreeIdxVar _ (GateArg _ Qbit) = return True

extendCtxWithGateParam :: GateArg -> EvaluationContext -> EvaluationContext
extendCtxWithGateParam (GateArg{..}) = M.insert name argType


findGateType :: Id -> EvaluationContext -> TypeCalculationResult
findGateType gateName = findTypeWithinScope gateName  >>> eitherFromPred isCircuit genIsNotGateErr
  where
    genIsNotGateErr :: TermType -> TypeErrAt
    genIsNotGateErr = flip ExpectedAGate gateName  >>> flip WithContext (extractCtx gateName)
    isCircuit :: TermType -> Bool
    isCircuit = L.has _Circuit



determineRegElemType :: TermType -> TermType
determineRegElemType (RegisterGroup Quantum _) = Qbit
determineRegElemType (RegisterGroup Classical _) = Bit

wrappedEitherFromPred :: Monad m => (a -> ExceptT err m Bool) -> (a -> err) -> a -> ExceptT err m a

wrappedEitherFromPred predicate errFn x =  predicate x >>= bool (fromEither $ Left $ errFn x) (return x)

-- Takes the context under which to evaluate an expression,
-- the index variables currently in-scope,
-- a parametric expression, and returns the type of the expression if it is
-- valid. Returns an error otherwise
verifyParametricExpr :: EvaluationContext -> [IndexVar] -> Expression -> TypeCalculationResult'
verifyParametricExpr m _ x@(Var{})  = verifyExpr' m x

verifyParametricExpr m validIdxVars RegisterAccess{registerName, registerNumber}
  = findTypeWithinScope' registerName m
  >>= wrappedEitherFromPred (isAccessingValidReg registerNumber validIdxVars) (error "Not handled yet")
  & fmap determineRegElemType
  where
    findTypeWithinScope' a = findTypeWithinScope a >>> fromEither
    isAccessingValidReg :: Idx -> [IndexVar] -> TermType -> ExceptT TypeErrAt IO Bool
    isAccessingValidReg wantedIdx inScopeIdxVars (RegisterGroup _ numOfRegs) = (proveAccessIsValid inScopeIdxVars `on` extractVal)  wantedIdx numOfRegs
    proveAccessIsValid :: [IndexVar] -> Index -> Index -> ExceptT TypeErrAt IO Bool
    proveAccessIsValid idxVarsInUse wantedIdx regCount =  ExceptT $ fmap (either (const (Right True)) (error "Have not implemented right branch yet")) $ proveNegationOf $ assumeIdxVarsAreNonNeg idxVarsInUse `G.symImplies` ((valueOf wantedIdx) `isBoundedExclusivelyBy` (valueOf regCount))

solveUsingZ3 :: G.SymBool -> IO (Either G.SolvingFailure G.Model)
solveUsingZ3 = G.solve G.z3

proveNegationOf :: G.SymBool -> IO (Either G.SolvingFailure G.Model)
proveNegationOf = G.symNot >>> solveUsingZ3

-- Takes two numbers, a, b, and generates a condition checking
-- that a is in [0, b)
isBoundedExclusivelyBy :: G.SymInteger -> G.SymInteger -> G.SymBool
isBoundedExclusivelyBy a lim = a G..>= 0 G..&& a G..< lim

-- Takes an index and returns the value represented by it
valueOf :: Index -> G.SymInteger

valueOf idx = indexVarTerms + constTerm
  where
    indexVars = L.view idxVarsCoefficients idx & M.toList
    indexVarTerms = indexVars
      & (L.each . L._1) L.%~ toSymVar
      & (L.each . L._2) L.%~ toSymInt
      & foldr (uncurry (*) >>> (+)) 0
    constTerm = L.view constPortion idx & toSymInt

-- Takes a collection of index variables that are in scope and
-- returns a term expressing the assumption that all of them are
-- non-negative
assumeIdxVarsAreNonNeg :: [IndexVar] -> G.SymBool
assumeIdxVarsAreNonNeg = foldr (genAndCombineConstraints . toSymVar) G.true
  where
    genAndCombineConstraints :: G.SymInteger -> G.SymBool -> G.SymBool
    genAndCombineConstraints = (G..>= 0) >>> (G..&&)

-- Takes the line where a gate was applied,
-- the types of the expected arguments for a gate,
-- the types of the actual arguments passed to the gate,
-- the arguments passed to the gate, and checks if the
-- expected and actual types match. Returns an error otherwise
verifyParametricGateArgs :: LineNumber -> [IndexVar] ->  TermType -> [TermType] -> [Expression] -> TypeCalculationResult'

verifyParametricGateArgs line validIdxVars (Circuit expectedArgTypes) actualArgTypes args
  | expectedArgTypes == actualArgTypes = return Unit
  | otherwise = error "Have not handled the case where a parametric gate application is invalid"

-- Takes the in-scope index variables, th ebody of a gate within a parametric gate declaration,
--  the context under which to evaluate the body, and returns an error if any part of the
-- gate body has an invalid type. Returns the overall type of the gate otherwise
verifyParametricGateBody ::[IndexVar] -> GateApp  -> EvaluationContext  -> TypeCalculationResult'

verifyParametricGateBody validIdxVars GateApp{gateId, gateArgs} m = do
  expectedTypes <- findGateType' gateId m
  actualTypes <- traverse (verifyParametricExpr m validIdxVars) gateArgs
  verifyParametricGateArgs (extractCtx gateId) validIdxVars expectedTypes actualTypes gateArgs
  where
    findGateType' a = findGateType a >>> fromEither

-- Takes a circuit family declaration, the context to evaluate it under, and
-- returns an error if the gate may take an empty collection. Approves the
-- declaration otherwise
verifyParametricGateDecl :: [IndexVar] -> GateInfo -> EvaluationContext -> TypeCalculationResult'
verifyParametricGateDecl validIndexVars GateInfo{args, gateBody} m = allM (isNotUsingFreeIdxVar validIndexVars) args *>  gateCtx >>= verifyParametricGateBody validIndexVars gateBody
  where
    verifyParametricTypeAnnotation :: GateArg -> ExceptT TypeErrAt IO GateArg
    verifyParametricTypeAnnotation arg@(GateArg collId (RegisterGroup _ numOfRegs)) = proveCollIsNonEmpty collId numOfRegs $> arg
    verifyParametricTypeAnnotation x = fromEither (Right x)
    gateCtx = foldr extendCtxWithGateParam m <$> traverse verifyParametricTypeAnnotation args


zero :: Index
zero = Index 0 M.empty

isPosIdx :: Idx -> Bool
isPosIdx = extractVal >>> (>= zero)

-- Takes the types of the parameters to a circuit and verifies
-- that each type is valid. Returns an error otherwise
verifyCircuitAnnotation :: [TermType] -> Either TypeErrAt [TermType]
verifyCircuitAnnotation = traverse verifyCircuitArg
  where
    verifyCircuitArg :: TermType -> TypeCalculationResult
    verifyCircuitArg x@(RegisterGroup _ numOfRegs)
      | isPosIdx numOfRegs = Right x
      | otherwise = toTypeErrAtLoc (InvalidCircuitAnnotation x) $ extractCtx numOfRegs
    verifyCircuitArg x@(Circuit argTypes) = traverse verifyCircuitArg argTypes $>  x
    verifyCircuitArg x = Right x

isNegIdx :: Idx -> Bool
isNegIdx = extractVal >>> (< zero)

isZero :: Idx -> Bool
isZero = extractVal >>> (== zero)

type TypeCalculationResult' = ExceptT TypeErrAt IO TermType


-- Takes information about a gate declaration, the local context, and
-- checks that the body of the gate is valid according to the
-- parameters in the declaration and the context. Returns an error otherwise
verifyGateDecl :: GateInfo -> EvaluationContext -> TypeCalculationResult'
verifyGateDecl GateInfo{..} m = (fromEither gateDeclCtx) >>= (`verifyGateApp'`  gateBody)
  where
    gateDeclCtx = foldr extendCtxWithGateParam m <$> traverse verifyTypeAnnotation args
    -- Checks that a type annotation is valid. Returns an error otherwise
    verifyTypeAnnotation :: GateArg -> Either TypeErrAt GateArg
    verifyTypeAnnotation arg@(GateArg regCollName (RegisterGroup collType numOfRegs))
      | isZero numOfRegs = genEmptyRegCollDeclErr  RegCollInfo {..}
      | isNegIdx numOfRegs = genNegLengthRegCollDeclErr  RegCollInfo {..}
      | otherwise = return arg

    verifyTypeAnnotation arg@(GateArg _ (Circuit argTypes)) = verifyCircuitAnnotation argTypes  $> arg

    verifyTypeAnnotation x  = return x


-- Takes information about a gate declaration, the context under which to evaluate the
-- declaration, a command, and evaluates the command with the gate type embedded in the context
-- if the declaration is valid. Returns an error otherwise
verifyOnlyIfGateDeclIsValid :: GateInfo -> EvaluationContext -> Command -> TypeCalculationResult'
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

toTypeCalculationResult :: TypeCalculationResult -> TypeCalculationResult'
toTypeCalculationResult = return >>> ExceptT

-- Takes a function determining the type of an expression that depends on a register collection,
-- information about the collection, and returns the type of the expression if the collection
-- and expression is valid. Returns an error otherwise
applyFIfRegCollDeclIsValid :: (RegCollInfo ->  TypeCalculationResult')  -> RegCollInfo -> TypeCalculationResult'
applyFIfRegCollDeclIsValid f info
  | isEmptyRegColl info = toTypeCalculationResult  $ genEmptyRegCollDeclErr info
  | isNegLengthColl info = toTypeCalculationResult $ genNegLengthRegCollDeclErr info
  | otherwise = f info
  where
    isNegLengthColl :: RegCollInfo -> Bool
    isNegLengthColl = numOfRegs >>> isNegIdx

-- Takes the current context, the makeup of a register collection
-- declaration, a command to evaluate, and evaluates the command under
-- the context updated with the declaration if an empty collection is not
-- being declared. Returns an error otherwise
evalIfRegCollDeclIsValid :: EvaluationContext -> RegCollInfo -> Command -> TypeCalculationResult'
evalIfRegCollDeclIsValid ctx declInfo toEval =  applyFIfRegCollDeclIsValid evalTermThatDependsOnRegColl declInfo
  where
    evalTermThatDependsOnRegColl = flip addRegCollToCtx ctx >>> (`verifyCommand` toEval)

    -- Takes the name and kind of a register collection along with the number of registers
    -- and updates the current evaluation context with the type of the collection
    addRegCollToCtx :: RegCollInfo -> EvaluationContext -> EvaluationContext
    addRegCollToCtx RegCollInfo{..} = M.insert regCollName (RegisterGroup collType numOfRegs)

extractCtx :: WithContext a b -> b
extractCtx (WithContext _ x) = x


-- Takes a function that generates an error about the size of a register collection,
-- information about a collection, and generates an error about the collection
-- using the function
genInvalidRegCollLengthErr :: (Identifier -> TypeEvaluationError) -> RegCollInfo -> Either TypeErrAt a
genInvalidRegCollLengthErr errFn RegCollInfo{..} =  toTypeErrAtLoc (errFn regCollName) $ extractCtx numOfRegs

genNegLengthRegCollDeclErr :: RegCollInfo -> Either TypeErrAt a
genNegLengthRegCollDeclErr = genInvalidRegCollLengthErr NegSizeRegCollDecl

isEmptyRegColl :: RegCollInfo -> Bool
isEmptyRegColl = numOfRegs >>> isZero

genEmptyRegCollDeclErr :: RegCollInfo -> Either TypeErrAt a
genEmptyRegCollDeclErr = genInvalidRegCollLengthErr EmptyRegCollDecl


verifyExpr' :: EvaluationContext -> Expression -> TypeCalculationResult'
verifyExpr' m = verifyExpr m >>> fromEither

-- Takes a context under which to evaluate an expression, an
-- expression, and returns the type of the evaluated expression if
-- possible. Returns an error otherwise explaining why the type
-- could not be determined
determineType :: EvaluationContext -> Term -> TypeCalculationResult'

determineType m term = term &
  (Vary.on @Expression (verifyExpr' m)
  $ Vary.on @GateApp (verifyGateApp' m)
  $ Vary.on @Command (verifyCommand m)
   $ Vary.exhaustiveCase  )
