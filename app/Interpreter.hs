module Interpreter (run, IS, Memory, PascalValue(..))  where

import qualified Parser as P
import Data.Map as Map
import Debug.Trace
import Control.Monad.State
import qualified Data.Text as T

data PascalValue = IntVal Int | FloatVal Float | NoVal
    deriving (Show)

type Scope = Map.Map P.VarId (PascalValue, P.TypeSpec)

type Memory = [Scope]

newtype IS = IS {
    memory :: Memory
} deriving (Show)

matchPascalValueToType :: PascalValue -> P.TypeSpec -> Bool
matchPascalValueToType (IntVal i) P.Integer = True
matchPascalValueToType (FloatVal f) P.Real = True
matchPascalValueToType _ _ = False

addScope :: State IS ()
addScope = modify (\s -> s { memory = Map.empty : memory s })

removeScope :: State IS ()
removeScope = modify (\s -> s { memory = tail (memory s) })

addVarToScope :: P.VarId -> P.TypeSpec -> State IS ()
addVarToScope varName varType = do
    currentState <- get 
    let currScope = head (memory currentState)
    let updatedScope = Map.insertWith const varName (NoVal, varType) currScope
    modify (\s -> s { memory = updatedScope : tail (memory currentState) })

setVarInScope :: P.VarId -> PascalValue -> Memory -> Memory -> State IS ()
setVarInScope varName _ _ [] = error ("Unknown Identifier " ++ show varName)
setVarInScope varName varVal prev (scope:scopes) = do
    case Map.lookup varName scope of
        Nothing -> setVarInScope varName varVal (prev ++ [scope]) scopes
        Just (_, varType) -> do
            if matchPascalValueToType varVal varType then do
                let updatedScope = Map.insertWith const varName (varVal, varType) scope
                modify (\s -> s { memory = prev ++ [updatedScope] ++ scopes })
            else
                error ("You are trying to assign something that is NOT a(n) " ++ show varType ++ " value to " ++ show varName)

setVar :: P.VarId -> PascalValue -> State IS ()
setVar varName varVal = do
    currentState <- get 
    setVarInScope varName varVal [] (memory currentState)

getVarInScope :: P.VarId -> Memory -> Memory -> State IS (PascalValue, P.TypeSpec)
getVarInScope varName _ [] = error ("Unknown Identifier " ++ show varName)
getVarInScope varName prev (scope:scopes) = do
    case Map.lookup varName scope of
        Nothing -> getVarInScope varName (prev ++ [scope]) scopes
        Just (NoVal, _) -> error ("Identifier not initialized " ++ show varName)
        Just val -> return val

getVar :: P.VarId -> State IS (PascalValue, P.TypeSpec)
getVar var = do
    currentState <- get
    getVarInScope var [] (memory currentState)

getVarValue :: P.VarId -> State IS PascalValue
getVarValue var = do
    varDef <- getVar var
    return (fst varDef)

getVarType :: P.VarId -> State IS P.TypeSpec
getVarType var = do
    varDef <- getVar var
    return (snd varDef)

evalFactor :: P.Factor -> State IS PascalValue
evalFactor (P.IntegerLiteral int) = return (IntVal int)
evalFactor (P.FloatLiteral float) = return (FloatVal float)
evalFactor (P.UnOperator op f) = case op of
    P.UnaryPlus -> evalFactor f
    P.UnaryMinus -> do
        evaledFactor <- evalFactor f
        return $ case evaledFactor of
            IntVal i -> IntVal (-i)
            FloatVal f -> FloatVal (-f)
evalFactor (P.Parens expr) = evalExpr expr
evalFactor (P.VarRef varName) = getVarValue varName

evalNumOperation :: (Int -> Int -> Int) -> (Float -> Float -> Float) ->  PascalValue -> PascalValue -> PascalValue
evalNumOperation opInt opFloat p1 p2 =
    case (p1, p2) of
        (IntVal i1, IntVal i2) -> IntVal (opInt i1 i2)
        (IntVal i, FloatVal f) -> FloatVal (opFloat (fromIntegral i) f)
        (FloatVal f, IntVal i) -> FloatVal (opFloat f (fromIntegral i))
        (FloatVal f1, FloatVal f2) -> FloatVal (opFloat f1 f2)

evalTerm :: P.Term -> State IS PascalValue
evalTerm (P.SingleFactor f) = evalFactor f
evalTerm (P.Mul f t) = do
    evaledFactor <- evalFactor f
    evaledTerm <- evalTerm t
    return (evalNumOperation (*) (*) evaledFactor evaledTerm)
-- I could abstract this into the evalNumOperation function, too, but it becomes less readable.
evalTerm (P.Div f t) = do
    p1 <- evalFactor f
    p2 <- evalTerm t
    return $ case (p1, p2) of
        (IntVal i1, IntVal i2) -> error "Use integer devision" -- Here it differs from evalNumeration
        (IntVal i, FloatVal f) -> FloatVal (fromIntegral i / f)
        (FloatVal f, IntVal i) -> FloatVal (f / fromIntegral i)
        (FloatVal f2, FloatVal f1) -> FloatVal (f1 / f2)

evalExpr :: P.Expr -> State IS PascalValue
evalExpr (P.SingleTerm t) = evalTerm t
evalExpr (P.Plus t e) = do
    evaledTerm <- evalTerm t
    evaledExpr <- evalExpr e
    return (evalNumOperation (+) (+) evaledTerm evaledExpr)
evalExpr (P.Minus t e) = do
    evaledTerm <- evalTerm t
    evaledExpr <- evalExpr e
    return (evalNumOperation (-) (-) evaledTerm evaledExpr)

evalAssignment :: P.VarId -> P.Expr -> State IS ()
evalAssignment varId expr = do
    evaledExpr <- evalExpr expr
    setVar varId evaledExpr
    return ()

evalStatement :: P.Statement -> State IS ()
evalStatement statement = do
    case statement of
        P.Empty -> return ()
        P.Assign varId expr -> evalAssignment varId expr
        P.CompoundStatement statements -> evalStatements statements

evalStatements :: P.StatementList -> State IS ()
evalStatements [] = return ()
evalStatements (x:xs) = do
    evalStatement x
    evalStatements xs

evalVarDec :: P.Declaration -> State IS ()
evalVarDec (P.VarDec ([], varType)) = return ()
evalVarDec (P.VarDec (id:varIds, varType)) = do
    addVarToScope id varType
    evalVarDec (P.VarDec (varIds, varType))
evalVarDec _ = return ()

evalVarDecls :: P.Declarations -> State IS ()
evalVarDecls [] = return ()
evalVarDecls (d:decls) = do
    evalVarDec d
    evalVarDecls decls

evalBlock :: P.Block -> State IS ()
evalBlock block = do
    evalTopLevelBlock block
    removeScope

-- this one doesn't pop the scope so we have something to look at
evalTopLevelBlock :: P.Block -> State IS ()
evalTopLevelBlock (P.Block decls statements) = do
    addScope
    evalVarDecls decls
    evalStatements statements

evalProgram :: P.Program -> State IS ()
evalProgram (P.Program progName block) = do
    evalTopLevelBlock block
    return ()

run :: P.Program -> Memory
run program = do
    let initialState = IS { memory = [] }
    let res = execState (evalProgram program) initialState
    memory res
