module Interpreter (run)  where

import qualified Parser as P

data PascalValue = IntVal Int | FloatVal Float | EvalError String
    deriving (Show)

evalFactor :: P.Factor -> PascalValue
evalFactor (P.IntegerLiteral int) = IntVal int
evalFactor (P.FloatLiteral float) = FloatVal float
evalFactor (P.UnOperator op f) = case op of
    P.UnaryPlus -> evalFactor f
    P.UnaryMinus -> do
        case evalFactor f of
            IntVal i -> IntVal (-i)
            FloatVal f -> FloatVal (-f)
            _ -> EvalError "Invalid type for operation"

evalFactor (P.Parens expr) = evalExpr expr

evalNumOperation :: (Int -> Int -> Int) -> (Float -> Float -> Float) ->  PascalValue -> PascalValue -> PascalValue
evalNumOperation opInt opFloat p1 p2 = 
    case (p1, p2) of
        (IntVal i1, IntVal i2) -> IntVal (opInt i1 i2)
        (IntVal i, FloatVal f) -> FloatVal (opFloat (fromIntegral i) f)
        (FloatVal f, IntVal i) -> FloatVal (opFloat f (fromIntegral i))
        (FloatVal f1, FloatVal f2) -> FloatVal (opFloat f1 f2)
        _ -> EvalError "Invalid type for operation"

evalTerm :: P.Term -> PascalValue
evalTerm (P.SingleFactor f) = evalFactor f
evalTerm (P.Mul f t) = evalNumOperation (*) (*) (evalFactor f) (evalTerm t)
-- I could abstract this into the evalNumOperation function, too, but it becomes less readable.
evalTerm (P.Div f t) = do
    let p1 = evalFactor f
    let p2 = evalTerm t
    case (p1, p2) of
        (IntVal i1, IntVal i2) -> EvalError "Use integer devision" -- Here it differs from evalNumeration
        (IntVal i, FloatVal f) -> FloatVal (fromIntegral i / f)
        (FloatVal f, IntVal i) -> FloatVal (f / fromIntegral i)
        (FloatVal f2, FloatVal f1) -> FloatVal (f1 / f2)
        _ -> EvalError "Invalid type for operation"

evalExpr :: P.Expr -> PascalValue
evalExpr (P.SingleTerm t) = evalTerm t
evalExpr (P.Plus t e) = evalNumOperation (+) (+) (evalTerm t) (evalExpr e)
evalExpr (P.Minus t e) = evalNumOperation (-) (-) (evalTerm t) (evalExpr e)

run :: IO ()
run = do
    res <- P.run
    case res of 
        Nothing -> print "No Expr"
        Just exp -> do
            let res1 = evalExpr exp
            print res1
    print (show res)