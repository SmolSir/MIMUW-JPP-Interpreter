{-# LANGUAGE RankNTypes #-}

module TypeChecker where

import Control.Monad.Reader
import Control.Monad.Trans.Except

import qualified Grammar.Abs as Abs

import qualified Data.Map as Map
import Data.List


------------------
-- types & data --
------------------
type Env = Map.Map String Type
type Position = Abs.BNFC'Position

type TypeCheckerT a = ExceptT String (Reader Env) a

data Type = IntType | StringType | BoolType | VoidType | FunctionType Type [Type] deriving Eq
instance Show Type where
    show = showType


------------------------------
-- general helper functions --
------------------------------
showType :: Type -> String
showType IntType    = "int"
showType StringType = "string"
showType BoolType   = "boolean"
showType VoidType   = "void"
showType (FunctionType returnType argumentTypeList) =
    (showType returnType) ++
    " function(" ++
    (intercalate ", " (map showType argumentTypeList)) ++
    ")"

parseType :: Abs.Type -> Type
parseType (Abs.Int _)  = IntType
parseType (Abs.Str _)  = StringType
parseType (Abs.Bool _) = BoolType
parseType (Abs.Void _) = VoidType
parseType (Abs.Fun _ returnType argumentTypeList) =
    FunctionType (parseType returnType) (map parseType argumentTypeList)

parseError :: forall a. String -> Position -> TypeCheckerT a
parseError errorMsg _ = throwE errorMsg

compareTypes :: Type -> Type -> String -> Position -> TypeCheckerT ()
compareTypes typeL typeR errorMsg position =
    if typeL == typeR then
        return ()
    else
        parseError errorMsg position


----------------------------------------
-- expression type checking functions --
----------------------------------------
expressionType :: Abs.Expr -> Env -> Either String Type
expressionType expression = runReader (runExceptT (checkExpressionType expression))

-- One operand version
checkOperandType :: Type -> Abs.Expr -> Type -> String -> Position -> TypeCheckerT Type
checkOperandType expectedType expression returnType errorMsg position = do
    operandType <- checkExpressionType expression
    if expectedType == operandType then
        return returnType
    else
        parseError (errorMsg ++ "not possible for type: " ++ show operandType) position

-- Two operands version
checkOperandsTypes :: Type -> Abs.Expr -> Abs.Expr -> Type -> String -> Position -> TypeCheckerT Type
checkOperandsTypes expectedType expressionL expressionR returnType errorMsg position = do
    operandTypeL <- checkExpressionType expressionL
    operandTypeR <- checkExpressionType expressionR
    if expectedType == operandTypeL && expectedType == operandTypeR then
        return returnType
    else
        parseError (errorMsg ++ show operandTypeL ++ " and " ++ show operandTypeR) position

checkExpressionType :: Abs.Expr -> TypeCheckerT Type
checkExpressionType (Abs.EVar _ (Abs.Ident identifier)) = do
    variableType <- asks (Map.lookup identifier)
    case variableType of
        Just varType -> return varType
        Nothing      -> throwE ("Not in scope: " ++ identifier)

checkExpressionType (Abs.ELitInt _ _) = return IntType

checkExpressionType (Abs.ELitTrue _) = return BoolType

checkExpressionType (Abs.ELitFalse _) = return BoolType

checkExpressionType (Abs.EApp _ (Abs.Ident functionIdentifier) argumentList) = do
    functionType      <- asks (Map.lookup functionIdentifier)
    expectedTypesList <- mapM checkExpressionType argumentList
    case functionType of
        Just function@(FunctionType returnType currentTypesList) ->
            if expectedTypesList == currentTypesList then
                return returnType
            else
                throwE (
                    "Incorrect call of " ++ show function ++ "with arguments (" ++
                    (intercalate ", " (map show currentTypesList))
                )
        Just _  -> throwE (show functionIdentifier ++ " is not callable")
        Nothing -> throwE (show functionIdentifier ++ " is not callable")

checkExpressionType (Abs.EString _ _) = return StringType

checkExpressionType (Abs.Neg position expression) =
    checkOperandType IntType expression IntType "Negation " position

checkExpressionType (Abs.Not position expression) =
    checkOperandType BoolType expression BoolType "Inversion " position

checkExpressionType (Abs.EMul position expressionL _ expressionR) =
    checkOperandsTypes
        IntType
        expressionL
        expressionR
        IntType
        "Multiplication / division types differ: "
        position

checkExpressionType (Abs.EAdd position expressionL _ expressionR) =
    checkOperandsTypes
        IntType
        expressionL
        expressionR
        IntType
        "Addition / subtraction types differ: "
        position

checkExpressionType (Abs.ERel position expressionL _ expressionR) =
    checkOperandsTypes
        IntType
        expressionL
        expressionR
        BoolType
        "Comparison types differ: "
        position

checkExpressionType (Abs.EAnd position expressionL expressionR) =
    checkOperandsTypes
        BoolType
        expressionL
        expressionR
        BoolType
        "Logical AND types differ: "
        position

checkExpressionType (Abs.EOr  position expressionL expressionR) =
    checkOperandsTypes
        BoolType
        expressionL
        expressionR
        BoolType
        "Logical OR types differ: "
        position


---------------------------------------
-- statement type checking functions --
---------------------------------------




checkProgram :: Abs.Program -> Except String ()
checkProgram = undefined
