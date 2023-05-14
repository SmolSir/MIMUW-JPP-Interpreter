{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Interpreter where

import System.IO

import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Fail

import qualified Grammar.Abs as Abs

import qualified Data.Map as Map
import Data.Maybe

------------------
-- types & data --
------------------
type Loc = Int
type Env = Map.Map String Loc
type Position = Abs.BNFC'Position

data Value =
    IntValue      Int    |
    BoolValue     Bool   |
    StringValue   String |
    VoidValue            |
    FunctionValue Function

data Function =
    FunctionByValue     (Value -> InterpreterT Function) |
    FunctionByReference (Loc -> InterpreterT Function)   |
    FunctionBottom      (InterpreterT Value)

data ReturnT = ReturnType Value | Break | Continue

data InterpreterState = InterpreterState {
    store   :: Map.Map Loc Value,
    prevLoc :: Loc
}

newtype InterpreterT a = InterpreterT {
    runInterpreterT :: ExceptT String (ReaderT Env (StateT InterpreterState IO)) a
} deriving (
    Applicative,
    Functor,
    Monad,
    MonadReader Env,
    MonadState InterpreterState,
    MonadFail)


------------------------------
-- general helper functions --
------------------------------
valueToInt :: Value -> Int
valueToInt (IntValue value) = value
valueToInt _                = undefined

valueToBool :: Value -> Bool
valueToBool (BoolValue value) = value
valueToBool _                 = undefined

valueToString :: Value -> String
valueToString (StringValue value) = value
valueToString _                   = undefined

valueToFunction :: Value -> Function
valueToFunction (FunctionValue value) = value
valueToFunction _                     = undefined

returnTToValue :: ReturnT -> Value
returnTToValue (ReturnType value) = value
returnTToValue _                  = undefined

justReturnT :: Value -> Maybe ReturnT
justReturnT = Just . ReturnType

allocateMemory :: InterpreterT Loc
allocateMemory =
    state (
        \currentState -> (
            prevLoc currentState + 1,
            currentState { prevLoc = prevLoc currentState + 1 }
        )
    )


getLoc :: String -> InterpreterT Loc
getLoc identifier = do
    loc <- asks (Map.lookup identifier)
    case loc of
        Just location -> return location
        Nothing       -> fail ("TODO" ++ identifier)

insertIntoStore :: Loc -> Value -> InterpreterT ()
insertIntoStore loc value =
    modify (\currentState -> currentState { store = Map.insert loc value (store currentState) })

getValue :: String -> Position -> InterpreterT Value
getValue identifier position = do
    loc <- getLoc identifier
    val <- gets (Map.lookup loc . store)
    case val of
        Just value -> return value
        Nothing    -> fail ("TODO")

parseError :: forall a. String -> Position -> InterpreterT a
parseError errorMsg position = fail ("TODO" ++ errorMsg)

-----------------------
-- execute functions --
-----------------------
execute :: [Abs.Stmt] -> InterpreterT (Maybe ReturnT)
execute [] = return Nothing

execute ((Abs.Empty _) : next) = execute next

execute ((Abs.BStmt _ (Abs.Block _ statements)) : next) = do
    result <- execute statements
    case result of
        Just _  -> return result
        Nothing -> execute next

execute ((Abs.Decl position itemType ((Abs.NoInit _ (Abs.Ident identifier)) : items)) : next) = do
    loc <- allocateMemory
    local (Map.insert identifier loc) (execute ((Abs.Decl position itemType items) : next))

execute ((Abs.Decl position itemType ((Abs.Init _ (Abs.Ident identifier) expression) : items)) : next) = do
    loc   <- allocateMemory
    value <- evaluate expression
    insertIntoStore loc value
    local (Map.insert identifier loc) (execute ((Abs.Decl position itemType items) : next))

execute ((Abs.Ass _ (Abs.Ident identifier) expression) : next) = do
    loc   <- getLoc identifier
    value <- evaluate expression
    insertIntoStore loc value
    execute next

execute ((Abs.RetVal _ expression) : _) = fmap justReturnT (evaluate expression)

execute ((Abs.RetVoid _) : _) = return (justReturnT VoidValue)

execute ((Abs.Cond _ expression statement) : next) = do
    condition <- evaluate expression
    if valueToBool condition then
        execute (statement : next)
    else
        execute next

execute ((Abs.CondElse _ expression statementTrue statementFalse) : next) = do
    condition <- evaluate expression
    if valueToBool condition then
        execute (statementTrue  : next)
    else
        execute (statementFalse : next)

execute loop@((Abs.While _ expression statement) : next) = do
    condition <- evaluate expression
    if valueToBool condition then do
        result <- execute [statement]
        case result of
            Just (ReturnType _) -> return result
            Just Break          -> execute next
            _                   -> execute loop
    else
        execute next

execute ((Abs.Continue _) : _) = return (Just Continue)

execute ((Abs.Break _) : _) = return (Just Break)

execute ((Abs.SExp _ expression) : next) = evaluate expression >> execute next


------------------------
-- evaluate functions --
------------------------
evaluate :: Abs.Expr -> InterpreterT Value
evaluate (Abs.EVar position (Abs.Ident identifier)) = getValue identifier position

evaluate (Abs.ELitInt _ integer) = return (IntValue (fromInteger integer))

evaluate (Abs.ELitTrue _) = return (BoolValue True)

evaluate (Abs.ELitFalse _) = return (BoolValue False)

evaluate (Abs.EApp position (Abs.Ident identifier) expressionList) = do
    function <- getValue identifier position
    apply (valueToFunction function) expressionList
    where
        apply :: Function -> [Abs.Expr] -> InterpreterT Value
        apply _ _ = undefined -- TODO!

evaluate (Abs.EString _ string) = return (StringValue string)

evaluate (Abs.Neg _ expression) = fmap IntValue (fmap (((-) 0) . valueToInt) (evaluate expression))

evaluate (Abs.Not _ expression) = fmap BoolValue (fmap (not . valueToBool) (evaluate expression))

evaluate (Abs.EMul position expressionL operator expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    apply (valueToInt valueL) operator (valueToInt valueR)
    where
        apply :: Int -> Abs.MulOp -> Int -> InterpreterT Value
        apply left (Abs.Times _) right = return (IntValue (left * right))
        apply _    (Abs.Div _)   0     = parseError "division by 0" position
        apply left (Abs.Div _)   right = return (IntValue (div left right))
        apply _    (Abs.Mod _)   0     = parseError "modulo by 0" position
        apply left (Abs.Mod _)   right = return (IntValue (mod left right))

evaluate (Abs.EAdd _ expressionL operator expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    apply (valueToInt valueL) operator (valueToInt valueR)
    where
        apply :: Int -> Abs.AddOp -> Int -> InterpreterT Value
        apply left (Abs.Plus _)  right = return (IntValue (left + right))
        apply left (Abs.Minus _) right = return (IntValue (left - right))

evaluate (Abs.ERel _ expressionL operator expressionR) = do
    valueL <- evaluate expressionL
    valueR <- evaluate expressionR
    apply (valueToInt valueL) operator (valueToInt valueR)
    where
        apply :: Int -> Abs.RelOp -> Int -> InterpreterT Value
        apply left (Abs.LTH _) right = return (BoolValue (left <  right))
        apply left (Abs.LE _)  right = return (BoolValue (left <= right))
        apply left (Abs.GTH _) right = return (BoolValue (left >  right))
        apply left (Abs.GE _)  right = return (BoolValue (left >= right))
        apply left (Abs.EQU _) right = return (BoolValue (left == right))
        apply left (Abs.NE _)  right = return (BoolValue (left /= right))

evaluate (Abs.EAnd _ expressionL expressionR) = do
    valueL <- evaluate expressionL
    if valueToBool valueL then
        evaluate expressionR
    else return (BoolValue False)

evaluate (Abs.EOr _ expressionL expressionR) = do
    valueL <- evaluate expressionL
    if valueToBool valueL then
        return (BoolValue True)
    else evaluate expressionR


----------------------------------
-- interpreter helper functions --
----------------------------------
getFunctionIdentifierList :: [Abs.TopDef] -> [String]
getFunctionIdentifierList topDefList =
    [identifier | (Abs.FnDef _ _ (Abs.Ident identifier) _ _) <- topDefList]

getFunctionDefList :: [Abs.TopDef] -> [([Abs.Arg], [Abs.Stmt])]
getFunctionDefList topDefList =
    [(arguments, body) | (Abs.FnDef _ _ _ arguments (Abs.Block _ body)) <- topDefList]

buildFunctionDef :: ([Abs.Arg], [Abs.Stmt]) -> InterpreterT Function
buildFunctionDef ((Abs.ArgByVal _ _ (Abs.Ident identifier)) : t, body) = do
    env <- ask
    return (
        FunctionByValue (
            \value -> local (const env) $ do
                loc <- allocateMemory
                insertIntoStore loc value
                local (Map.insert identifier loc) (buildFunctionDef (t, body))
            )
        )

buildFunctionDef ((Abs.ArgByRef _ _ (Abs.Ident identifier)) : t, body) = do
    env <- ask
    return (
        FunctionByReference (
            \loc -> local (const env) $ do
                local (Map.insert identifier loc) (buildFunctionDef (t, body))
            )
        )

buildFunctionDef ([], body) = do
    env <- ask
    return (
        FunctionBottom (
                fmap (returnTToValue . fromJust) (local (const env) (execute body))
            )
        )

registerDefaultFunctions :: InterpreterT () -> InterpreterT ()
registerDefaultFunctions = undefined -- TODO!

registerDefinedFunctions :: [(Loc, InterpreterT Function)] -> InterpreterT ()
registerDefinedFunctions [] = do
    FunctionValue (FunctionBottom main) <- getValue "main" Nothing
    main >> return ()

registerDefinedFunctions ((loc, function) : t) = do
    env <- ask
    fun <- function
    insertIntoStore loc (FunctionValue fun)
    registerDefinedFunctions t

-------------------------------
-- interpreter run functions --
-------------------------------
buildInterpreter :: Abs.Program -> InterpreterT ()
buildInterpreter (Abs.Program _ topDefList) = do
    locList <- replicateM (length topDefList) allocateMemory
    let env = Map.union (Map.fromList (zip (getFunctionIdentifierList topDefList) locList)) in
        let functionList = zip locList (map buildFunctionDef (getFunctionDefList topDefList)) in
            local env (registerDefaultFunctions $ (registerDefinedFunctions functionList))
        -- TODO! local env (registerDefaultFunctions $ registerDefinedFunctions [(loc, buildFunctionDef function) | (loc, function) <- zip locList (getFunctionDefList topDefList)])

runInterpreter :: InterpreterT a -> IO (Either String a)
runInterpreter interpreter =
    evalStateT runResult initialState
    where
        runResult    = runReaderT (runExceptT (runInterpreterT interpreter)) Map.empty
        initialState = InterpreterState { store = Map.empty, prevLoc = 0 }

runProgram :: Abs.Program -> IO ( )
runProgram program = do
    returnValue <- runInterpreter $ buildInterpreter program
    case returnValue of
        Left  errorMsg -> hPutStrLn stderr ("TODO: " ++ errorMsg)
        Right _        -> return ()
