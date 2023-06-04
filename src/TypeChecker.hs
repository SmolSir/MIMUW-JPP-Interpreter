{-# LANGUAGE RankNTypes #-}

module TypeChecker where

import Control.Monad (foldM)
import Control.Monad.Reader
import Control.Monad.Trans.Except

import qualified Grammar.Abs as Abs

import qualified Data.Map as Map
import Data.List
import Data.Functor.Identity


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
showPosition :: Position -> String
showPosition Nothing = "[unknown]"
showPosition (Just (line, column)) = "[Ln " ++ show line ++ ", Col " ++ show column ++ "] "

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
parseError errorMsg position = throwE ((showPosition position) ++ errorMsg)

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
addDeclarationToEnv :: Env -> Abs.Stmt -> Except String Env
addDeclarationToEnv env (Abs.Decl _ declarationType itemList) =
    case sequence [expressionType expression env
                        | Abs.Init _ (Abs.Ident _) expression <- itemList] of
        Left  errorMsg -> throwE errorMsg
        Right _        -> do
            let variableEnv = Map.fromList
                    (getVariableListOfType (parseType declarationType) itemList) in
                if (Map.size variableEnv == length itemList) &&
                    (Map.null (Map.intersection variableEnv env))
                then
                    return (Map.union variableEnv env)
                else
                    throwE "Local variable redeclared within a block."
    where
        getVariableListOfType :: Type -> [Abs.Item] -> [(String, Type)]
        getVariableListOfType variableType variableList =
            [(identifier, variableType) | Abs.Init _ (Abs.Ident identifier) _ <- variableList] ++
            [(identifier, variableType) | Abs.NoInit _ (Abs.Ident identifier) <- variableList]

addDeclarationToEnv _ nonDeclStatement =
    throwE ("Unexpected attempt of adding " ++ show nonDeclStatement ++ " to the type environment")

getDeclarationList :: Env -> Abs.Block -> Except String (Env, Abs.Block)
getDeclarationList env block@(Abs.Block _ allDeclarationList) = do
    let (declarationList, nonDeclarationList) =
            (filter isDeclaration allDeclarationList,
             declarationToAssignmentList =<< allDeclarationList) in
        case runIdentity (runExceptT (foldM addDeclarationToEnv env declarationList)) of
            Left  errorMsg  -> throwE errorMsg
            Right resultEnv -> return (resultEnv, Abs.Block (Abs.hasPosition block) nonDeclarationList)
    where
        isDeclaration :: Abs.Stmt -> Bool
        isDeclaration (Abs.Decl _ _ _) = True
        isDeclaration _                = False

        declarationToAssignmentList :: Abs.Stmt -> [Abs.Stmt]
        declarationToAssignmentList (Abs.Decl _ _ itemList) =
            [Abs.Ass position identifier expression
                | (Abs.Init position identifier expression) <- itemList]
        declarationToAssignmentList other = [other]

checkStatementType :: Abs.Stmt -> TypeCheckerT ()
checkStatementType (Abs.Empty _) = return ()

checkStatementType (Abs.BStmt position block@(Abs.Block _ _)) = do
    env <- ask
    case runExcept (getDeclarationList env block) of
        Left errorMsg -> parseError errorMsg position
        Right (resultEnv, Abs.Block _ itemList) -> local (const resultEnv) (check itemList)
    where
        check []      = return ()
        check (statement : rest) = checkStatementType statement >> check rest

checkStatementType statement@(Abs.Ass _ (Abs.Ident identifier) expression) = do
    env <- ask
    case (Map.lookup identifier env, expressionType expression env) of
        (_                  , Left errorMsg) ->
            parseError errorMsg (Abs.hasPosition statement)
        (Nothing,             Right _) ->
            parseError ("Undefined variable: " ++ identifier) (Abs.hasPosition statement)
        (Just identifierType, Right exprType) ->
            compareTypes
                identifierType
                exprType
                ("Cannot assign value of type: " ++ show exprType ++
                    " to a variable of type: " ++ show identifierType)
                (Abs.hasPosition statement)

checkStatementType statement@(Abs.RetVal _ expression) = do
    env <- ask
    case (Map.lookup "return" env, expressionType expression env) of
        (_, Left errorMsg) ->
            parseError errorMsg (Abs.hasPosition statement)
        (Nothing, Right _) ->
            parseError "Error: missing return statement" (Abs.hasPosition statement)
        (Just returnType, Right exprType) ->
            compareTypes
                returnType
                exprType
                ("Cannot return value of type: " ++ show exprType ++
                    " from a function of type: " ++ show returnType)
                (Abs.hasPosition statement)

checkStatementType statement@(Abs.RetVoid _) = do
    env <- ask
    case (Map.lookup "return" env) of
        Just VoidType -> return ()
        _             ->
            parseError "Cannot return value from a void function" (Abs.hasPosition statement)

checkStatementType statement@(Abs.Cond _ expression statementTrue) = do
    env <- ask
    case (expressionType expression env) of
        Left errorMsg  -> parseError errorMsg (Abs.hasPosition statement)
        Right BoolType -> return ()
        Right _        -> parseError "Error: non-boolean condition" (Abs.hasPosition statement)
    checkStatementType statementTrue

checkStatementType statement@(Abs.CondElse _ expression statementTrue statementFalse) = do
    env <- ask
    case (expressionType expression env) of
        Left errorMsg  -> parseError errorMsg (Abs.hasPosition statement)
        Right BoolType -> return ()
        Right _        -> parseError "Error: non-boolean condition" (Abs.hasPosition statement)
    checkStatementType statementTrue
    checkStatementType statementFalse

checkStatementType statement@(Abs.While _ expression statementLoop) = do
    env <- ask
    case (expressionType expression env) of
        Left errorMsg  -> parseError errorMsg (Abs.hasPosition statement)
        Right BoolType -> return ()
        Right _        -> parseError "Error: non-boolean condition" (Abs.hasPosition statement)
    local (Map.insert "insideLoop" BoolType) (checkStatementType statementLoop)

checkStatementType statement@(Abs.Continue _) = do
    insideLoop <- asks (Map.lookup "insideLoop")
    case insideLoop of
        Just _  -> return ()
        Nothing -> parseError "Continue outside of a loop" (Abs.hasPosition statement)

checkStatementType statement@(Abs.Break _) = do
    insideLoop <- asks (Map.lookup "insideLoop")
    case insideLoop of
        Just _  -> return ()
        Nothing -> parseError "Break outside of a loop" (Abs.hasPosition statement)

checkStatementType statement@(Abs.SExp _ expression) = do
    env <- ask
    case (expressionType expression env) of
        Left errorMsg -> parseError errorMsg (Abs.hasPosition statement)
        Right _       -> return ()

checkStatementType other =
    throwE ("Error: unexpected call of checkStatementType with argument: " ++ show other)


----------------------------------
-- typechecker helper functions --
----------------------------------
functionArgumentType :: Abs.Arg -> Abs.Type
functionArgumentType (Abs.ArgByVal _ valueType     _) = valueType
functionArgumentType (Abs.ArgByRef _ referenceType _) = referenceType

functionArgumentName :: Abs.Arg -> String
functionArgumentName (Abs.ArgByVal _ _ (Abs.Ident identifier)) = identifier
functionArgumentName (Abs.ArgByRef _ _ (Abs.Ident identifier)) = identifier

functionArgumentNameList :: Abs.TopDef -> [(String, Type)]
functionArgumentNameList (Abs.FnDef _ _ _ argumentList _) =
    zip (map functionArgumentName argumentList) (map (parseType . functionArgumentType) argumentList)

defaultFunctionList :: [(String, Type, Position)]
defaultFunctionList = [
    ("print", FunctionType VoidType [IntType], Nothing),
    ("println", FunctionType VoidType [IntType], Nothing),
    ("printString", FunctionType VoidType [StringType], Nothing),
    ("printlnString", FunctionType VoidType [StringType], Nothing),
    ("printBoolean", FunctionType VoidType [BoolType], Nothing),
    ("printlnBoolean", FunctionType VoidType [BoolType], Nothing)
    ]

parseTopLevelFunction :: Abs.TopDef -> (String, Type, Position)
parseTopLevelFunction (Abs.FnDef position returnType (Abs.Ident identifier) argumentList _) =
    (identifier,
     FunctionType (parseType returnType) (map (parseType . functionArgumentType) argumentList),
     position)

mapTopLevelFunctionList :: [(String, Type, Position)] -> Either String (Map.Map String Type)
mapTopLevelFunctionList functionList = parseList functionList (return Map.empty)
    where
        parseList [] accFunction = accFunction
        parseList ((identifier, functionType, position) : rest) accFunction = do
            functionMap <- accFunction
            if (Map.member identifier functionMap) then
                Left (
                    "redefinition of identifier: " ++ show identifier ++
                    "at position: " ++ showPosition position
                )
            else
                parseList rest (return (Map.insert identifier functionType functionMap))

-------------------------------
-- typechecker run functions --
-------------------------------
checkProgram :: Abs.Program -> Except String ()
checkProgram (Abs.Program _ functionList) = do
    let topLevelFunctionList = map parseTopLevelFunction functionList in
        case (mapTopLevelFunctionList (defaultFunctionList ++ topLevelFunctionList)) of
            Left errorMsg -> throwE errorMsg
            Right functionDefinitionList ->
                if (Map.lookup "main" functionDefinitionList /= Just (FunctionType IntType [])) then
                    throwE "Error: 'main()' function missing"
                else
                    checkFunctionList functionList functionDefinitionList topLevelFunctionList

checkFunctionList :: [Abs.TopDef] -> Env -> [(String, Type, Position)] -> Except String ()
checkFunctionList [] _ _ = return ()

checkFunctionList
        (functionDefinition@(Abs.FnDef position _ _ _ block) : restDefs)
        env
        ((_, (FunctionType returnType _), _) : restTypes) = do
    case runIdentity (runReaderT (runExceptT (checkStatementType (Abs.BStmt position block))) currentEnv) of
        Left errorMsg -> throwE errorMsg
        Right _       -> checkFunctionList restDefs env restTypes
    where
        currentEnv = Map.union
            (Map.insert "return" returnType env)
            (Map.fromList (functionArgumentNameList functionDefinition))

checkFunctionList _ _ _ = throwE "Error: Unexpected pattern in function checkFunctionList"
