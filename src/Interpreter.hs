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
intValue :: Value -> Int
intValue (IntValue value) = value
intValue _                = undefined

boolValue :: Value -> Bool
boolValue (BoolValue value) = value
boolValue _                 = undefined

stringValue :: Value -> String
stringValue (StringValue value) = value
stringValue _                   = undefined

functionValue :: Value -> Function
functionValue (FunctionValue value) = value
functionValue _                     = undefined

returnTValue :: ReturnT -> Value
returnTValue (ReturnType value) = value
returnTValue _                  = undefined

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
    if boolValue condition then
        execute (statement : next)
    else
        execute next

execute ((Abs.CondElse _ expression statementTrue statementFalse) : next) = do
    condition <- evaluate expression
    if boolValue condition then
        execute (statementTrue  : next)
    else
        execute (statementFalse : next)

execute loop@((Abs.While _ expression statement) : next) = do
    condition <- evaluate expression
    if boolValue condition then do
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

execute _ = undefined


------------------------
-- evaluate functions --
------------------------
evaluate :: Abs.Expr -> InterpreterT Value
evaluate _ = undefined


----------------------------------
-- interpreter helper functions --
----------------------------------
-- getFunctions :: Abs.TopDef -> ([Loc], [(Abs.Ident, ([Abs.Arg], Abs.Block))])

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
                fmap (returnTValue . fromJust) (local (const env) (execute body))
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
