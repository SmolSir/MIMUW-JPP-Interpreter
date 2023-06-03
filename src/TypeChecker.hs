module TypeChecker where

import Control.Monad.Reader
import Control.Monad.Except

import qualified Grammar.Abs as Abs

import qualified Data.Map as Map

------------------
-- types & data --
------------------
type Env = Map.Map String Type
type Position = Abs.BNFC'Position

data Type = IntType | StringType | BoolType | VoidType | FunctionType Type [Type]

type TypeCheckerT a = ExceptT String (Reader Env) a



checkProgram :: Abs.Program -> Except String ()
checkProgram = undefined
