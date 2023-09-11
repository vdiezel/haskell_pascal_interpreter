module Interpreter (run, IS, Memory, PascalValue(..))  where

import qualified Parser as P
import Data.Map as Map
import Debug.Trace
import Control.Monad.State
import qualified Data.Text as T

data PascalValue = IntVal Int | FloatVal Float
    deriving (Show)

type Memory = Map.Map P.VarId PascalValue
type SymbolicTable = Map.Map P.VarId P.TypeSpec

data IS = IS {
    memory :: Memory,
    symbolicTable :: SymbolicTable
} deriving (Show)

matchPascalValueToType :: PascalValue -> P.TypeSpec -> Bool
matchPascalValueToType (IntVal i) P.Integer = True
matchPascalValueToType (FloatVal f) P.Real = True
matchPascalValueToType _ _ = False

-- TODO: add type check
setVar :: P.VarId -> PascalValue -> State IS ()
setVar var val = do
    varType <- getVarType var
    if matchPascalValueToType val varType then do
        modify (\s -> s { memory = Map.insertWith (\_ new -> new) var val (memory s)  })
    else
        error ("You are trying to assign something that is NOT a(n) " ++ show varType ++ " value to " ++ show var)

setVarType :: P.VarId -> P.TypeSpec -> State IS ()
setVarType var varType = modify (\s -> s { symbolicTable = Map.insertWith (\_ new -> new) var varType (symbolicTable s)  })

getVar :: P.VarId -> State IS PascalValue
getVar var = do
    currentState <- get
    case Map.lookup var (memory currentState) of
        Nothing -> error ("Unknown Identifier " ++ show var)
        Just val -> return val

getVarType :: P.VarId -> State IS P.TypeSpec
getVarType var = do
    currentState <- get
    case Map.lookup var (symbolicTable currentState) of
        Nothing -> error ("Unknown Identifier " ++ show var)
        Just val -> return val

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
evalFactor (P.VarRef varName) = getVar varName

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

evalBlock :: P.Block -> State IS ()
evalBlock (P.Block decls statements) = do
    evalVarDecls decls
    evalStatements statements

evalVarDec :: P.Declaration -> State IS ()
evalVarDec (P.VarDec ([], varType)) = return ()
evalVarDec (P.VarDec (id:varIds, varType)) = do
    setVarType id varType
    evalVarDec (P.VarDec (varIds, varType))
evalVarDec _ = return ()

evalVarDecls :: P.Declarations -> State IS ()
evalVarDecls [] = return ()
evalVarDecls (d:decls) = do
    evalVarDec d
    evalVarDecls decls

evalProgram :: P.Program -> State IS ()
evalProgram (P.Program progName block) = do
    evalBlock block
    return ()

run :: P.Program -> Memory
run program = do
    let initialState = IS { symbolicTable = Map.empty, memory = Map.empty }
    let res = execState (evalProgram program) initialState
    trace (show (symbolicTable res)) $ memory res
