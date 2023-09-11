module Parser where

import qualified Data.Sequence as Seq
import Control.Monad.State
import Debug.Trace
import qualified Data.Text as T
import Lexer as L

-- TODO: Figure out how to use Applicative/Monad for Maybe
-- to prevent my deep nesting to handle "Nothing"

data PS = PS {
  tokens :: Seq.Seq L.Token,
  tokenIndex :: Int
} deriving (Show)

data UnOp = UnaryPlus | UnaryMinus
  deriving (Show)

data Program = Program VarId Block
  deriving (Show)

data Block = Block Declarations StatementList
  deriving (Show)

type StatementList = [Statement]

type Declarations = [Declaration]
data TypeSpec = Integer | Real
  deriving (Show)
type VarDec = ([VarId], TypeSpec)
data Declaration = VarDec VarDec | ProcDec VarId Block
  deriving(Show)

type VarId = T.Text
data Statement = CompoundStatement StatementList
    | Assign VarId Expr
    | Empty
    deriving (Show)

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
            | UnOperator UnOp Factor
            | Parens Expr
            | VarRef VarId
             deriving (Show)

consumeNext :: State PS ()
consumeNext = modify (\s -> s { tokenIndex = tokenIndex s + 1 })

getNextToken :: State PS (Maybe L.Token)
getNextToken = do
  currentState <- get
  let idx = tokenIndex currentState
  if idx >= Seq.length (tokens currentState) then return Nothing
  else return (Just (Seq.index (tokens currentState) idx))

parseUnaryOp :: UnOp -> State PS (Maybe Factor)
parseUnaryOp unOp = do
  consumeNext
  mFactor <- parseFactor
  case mFactor of
    Nothing -> return Nothing
    Just factor -> return (Just (UnOperator unOp factor))

parseFactor :: State PS (Maybe Factor)
parseFactor = do
  mToken <- getNextToken
  case mToken of
    Just (IntegerVal int) -> consumeNext >> return (Just (IntegerLiteral int))
    Just (FloatVal f) -> consumeNext >> return (Just (FloatLiteral f))
    Just L.Plus -> parseUnaryOp UnaryPlus
    Just L.Minus -> parseUnaryOp UnaryMinus
    Just (L.Id varName) -> consumeNext >> return (Just (VarRef varName))
    Just L.Lparen -> do
      consumeNext
      mExpression <- parseExpr
      case mExpression of
        Nothing -> return Nothing
        Just expression -> do
          mNextToken <- getNextToken
          case mNextToken of
            Just L.Rparen -> consumeNext >> return (Just (Parens expression))
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
        Just L.Mult -> do consumeNext >> composeTerm Mul factor
        Just L.Div -> do consumeNext >> composeTerm Parser.Div factor
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
        Just L.Plus -> do consumeNext >> composeExpr Parser.Plus term
        Just L.Minus -> do consumeNext >> composeExpr Parser.Minus term
        _ -> return (Just (SingleTerm term))
    _ -> return Nothing

parseStatement :: State PS (Maybe Statement)
parseStatement = do
  mToken <- getNextToken
  case mToken of
    Just Begin -> do
      mStatementList <- parseCompoundStatement
      case mStatementList of
        Nothing -> return Nothing
        Just statementList -> return (Just (CompoundStatement statementList))
    Just (Id varName) -> do
      consumeNext
      mNextToken <- getNextToken
      case mNextToken of
        Just L.Assign -> do
          consumeNext
          mExpr <- parseExpr
          case mExpr of
            Just expr -> return (Just (Parser.Assign varName expr))
            _ -> return Nothing
        _ -> return (Just Empty)
    _ -> return (Just Empty)

parseStatementList :: StatementList -> State PS (Maybe StatementList)
parseStatementList currList = do
  mStatement <- parseStatement
  case mStatement of
    Just Empty -> return (Just (currList ++ [Empty]))
    Just statement -> do
      mNextToken <- getNextToken
      case mNextToken of
        Just L.Semi -> consumeNext >> parseStatementList (currList ++ [statement])
        _ -> return (Just (currList ++ [statement]))
    Nothing -> return Nothing

parseVarIds :: [VarId] -> State PS (Maybe [VarId])
parseVarIds currIds = do
  mIdToken <- getNextToken
  case mIdToken of
    Just (Id varName) -> do
      consumeNext
      mNextToken <- getNextToken
      case mNextToken of
        Just L.Comma -> consumeNext >> parseVarIds (currIds ++ [varName])
        _ -> return (Just (currIds ++ [varName]))
    _ -> return Nothing

getTypeSpecType :: Token -> Maybe TypeSpec
getTypeSpecType L.Integer = Just Parser.Integer
getTypeSpecType L.Real = Just Parser.Real
getTypeSpecType _ = Nothing

-- There must be a way to define something that can prevent this deep nesting
-- and constant fallback to nothing, prob. Applicative, however there are
-- always some edge cases that make it less straight forward

parseDeclarations :: Declarations -> State PS (Maybe Declarations)
parseDeclarations currDec = do
    mNextToken <- getNextToken
    case mNextToken of
      Just L.Begin -> return (Just currDec)
      Just L.Procedure -> do
        consumeNext
        mProcId <- getNextToken
        case mProcId of
          Just (Id procName) -> do
            consumeNext
            mSemi <- getNextToken
            case mSemi of 
              Just L.Semi -> do
                consumeNext
                mBlock <- parseBlock
                case mBlock of
                  Just block -> do 
                    mSemiClosing <- getNextToken
                    case mSemiClosing of
                      Just L.Semi -> do
                        consumeNext
                        parseDeclarations (currDec ++ [ProcDec procName block])
                      _ -> return Nothing
                  _ -> return Nothing
              _ -> return Nothing
          _ -> return Nothing
      _ -> do
        mVarIds <- parseVarIds []
        case mVarIds of
          Just ids -> do
            mColonToken <- getNextToken
            case mColonToken of
              Just L.Colon -> do
                consumeNext
                mTypeToken <- getNextToken
                case mTypeToken of
                  Nothing -> return Nothing
                  Just token -> do
                    let mTypeSpec = getTypeSpecType token
                    case mTypeSpec of
                      Just typeSpec -> do
                        consumeNext
                        mSemiToken <- getNextToken
                        case mSemiToken of
                          Just L.Semi -> do
                            consumeNext
                            parseDeclarations (currDec ++ [VarDec (ids, typeSpec)])
                          Nothing -> return Nothing
                      Nothing -> return Nothing
              Nothing -> return Nothing
          Nothing -> return Nothing

parseDeclarationBlock :: State PS (Maybe Declarations)
parseDeclarationBlock = do
  mVarToken <- getNextToken
  case mVarToken of
    Just L.Var -> do
      consumeNext
      parseDeclarations []
    _ -> return (Just [])

parseCompoundStatement :: State PS (Maybe StatementList)
parseCompoundStatement = do
  mNextToken <- getNextToken
  case mNextToken of
    Just L.Begin -> do
      consumeNext
      mStatementList <- parseStatementList []
      case mStatementList of
        Nothing -> return Nothing
        Just statementList -> do
          mEndToken <- getNextToken
          case mEndToken of
            Just L.End -> consumeNext >> return (Just statementList)
            _ -> return Nothing
    _ -> return Nothing

parseBlock :: State PS (Maybe Block)
parseBlock = do
  mDecls <- parseDeclarationBlock
  case mDecls of
    Nothing -> return Nothing
    Just decls -> do
      mStatementList <- parseCompoundStatement
      case mStatementList of
        (Just statements) -> return (Just (Parser.Block decls statements))
        _ -> return Nothing

parseProgram :: State PS (Maybe Program)
parseProgram = do
  mToken <- getNextToken
  case mToken of
    (Just L.Program) -> do
      consumeNext
      mVarToken <- getNextToken
      case mVarToken of
        Just (Id varName) -> do
          consumeNext
          mSemiToken <- getNextToken
          case mSemiToken of
            Just L.Semi -> do
              consumeNext
              mBlock <- parseBlock
              mNextToken <- getNextToken
              case (mBlock, mNextToken) of
                (Just block, Just L.Dot) -> do
                  consumeNext
                  return (Just (Parser.Program varName block))
                _ -> return Nothing
            _ -> return Nothing
        _ -> return Nothing
    _ -> return Nothing

run :: Seq.Seq Token -> Maybe Program
run tokens = do
  case Seq.index tokens (Seq.length tokens - 1) of
    Error error -> Nothing
    _ -> do
      let initialState = PS { tokens = tokens, tokenIndex = 0 }
      let (program, state) = runState parseProgram initialState
      program
