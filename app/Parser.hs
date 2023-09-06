module Parser where

import qualified Data.Sequence as Seq
import Control.Monad.State
import Debug.Trace
import qualified Data.Text as T
import Lexer as L

data PS = PS {
  tokens :: Seq.Seq L.Token,
  tokenIndex :: Int
} deriving (Show)

data Expr = SingleTerm Term 
            | Plus Term Expr
            | Minus Term Expr
             deriving (Show)
    
data Term = SingleFactor Factor
            | Mul Factor Term
            | Div Factor Term
             deriving (Show)

data Factor = IntegerLiteral Int
            | FloatLiteral Float
            | Parens Expr
             deriving (Show)

advance :: State PS ()
advance = modify (\s -> s { tokenIndex = tokenIndex s + 1 })

getNextToken :: State PS (Maybe L.Token)
getNextToken = do
  currentState <- get
  let idx = tokenIndex currentState
  if idx >= Seq.length (tokens currentState) then return Nothing
  else return (Just (Seq.index (tokens currentState) idx))

parseFactor :: State PS (Maybe Factor)
parseFactor = do
  mToken <- getNextToken
  case mToken of 
    Just (IntegerVal int) -> advance >> return (Just (IntegerLiteral int))
    Just (FloatVal f) -> advance >> return (Just (FloatLiteral f))
    Just L.Lparen -> do
      advance
      mExpression <- parseExpr
      case mExpression of
        Nothing -> return Nothing
        Just expression -> do
          mNextToken <- getNextToken
          case mNextToken of 
            Just L.Rparen -> advance >> return (Just (Parens expression))
            _ -> return Nothing
    _ -> return Nothing

composeTerm :: (Factor -> Term -> Term) -> Factor -> State PS (Maybe Term)
composeTerm op factor = do
  mNextTerm <- parseTerm
  case mNextTerm of
    Nothing -> return Nothing
    Just nextTerm -> return (Just (op factor nextTerm))

parseTerm :: State PS (Maybe Term)
parseTerm = do
  mFactor <- parseFactor
  case mFactor of
    Just factor -> do
      mOperator <- getNextToken
      case mOperator of
        Just L.Mult -> do advance >> composeTerm Mul factor
        Just L.Div -> do advance >> composeTerm Parser.Div factor
        _ -> return (Just (SingleFactor factor))
    _ -> return Nothing

composeExpr :: (Term -> Expr -> Expr) -> Term -> State PS (Maybe Expr)
composeExpr op term = do
  mNextExpr <- parseExpr
  case mNextExpr of
    Nothing -> return Nothing
    Just nextExpr -> return (Just (op term nextExpr))

parseExpr :: State PS (Maybe Expr)
parseExpr = do
  mTerm <- parseTerm
  case mTerm of
    Just term -> do
      mOperator <- getNextToken
      case mOperator of
        Just L.Plus -> do advance >> composeExpr Parser.Plus term
        Just L.Minus -> do advance >> composeExpr Parser.Minus term
        _ -> return (Just (SingleTerm term))
    _ -> return Nothing

run :: IO (Maybe Expr)
run = do
  tokens <- L.run "./programs/program2.pas"

  case Seq.index tokens (Seq.length tokens - 1) of
    Error error -> return Nothing
    _ -> do
      let initialState = PS { tokens = tokens, tokenIndex = 0 }
      let (res, state) = runState parseExpr initialState
      -- print (show state)
      return res