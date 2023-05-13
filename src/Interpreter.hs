module Interpreter where

import System.IO

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import qualified Grammar.Abs as Abs

import qualified Data.Map as Map

type Loc = Int
type Env = Map.Map String Loc

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

data InterpreterState = InterpreterState {
    store   :: Map.Map Loc Value,
    prevLoc :: Loc
}

newtype InterpreterT a = InterpreterT {
    runInterpreterT :: ExceptT String (ReaderT Env (StateT InterpreterState IO)) a
}


buildInterpreter :: Abs.Program -> InterpreterT ()
buildInterpreter = undefined

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
