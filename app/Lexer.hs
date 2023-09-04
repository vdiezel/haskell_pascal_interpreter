{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Writing this rather imperatively for performance reasons
-- and easier access to the exaxt point of failure in case of failure

module Lexer (run, Token (..)) where

import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA
import Data.Char
import qualified Data.Sequence as Seq
import Debug.Trace
import Data.Maybe

type RawProgram = T.Text
type Keyword = T.Text

programKeyword = "program" :: Keyword
beginKeyword = "begin" :: Keyword
varKeyword = "var" :: Keyword
endKeyword = "end" :: Keyword
integerKeyword = "integer" :: Keyword
realKeyword = "real" :: Keyword

data Token = IntegerVal Int
  | FloatVal Float
  | Plus
  | Mult
  | Minus
  | Div
  | Dot
  | IntDiv
  | Integer
  | Real
  | Assign
  | Semi
  | Comma
  | Colon
  | Lparen
  | Rparen
  | Program
  | Var
  | Id T.Text
  | Begin
  | End
  | Error T.Text
  | EOF
  deriving (Show)

-- LexicalAnalysisState
data LAS = LAS {
  text :: RawProgram,
  pos :: Int,
  col :: Int,
  row :: Int,
  tokens :: Seq.Seq Token
} deriving (Show)

move :: Int -> State LAS ()
move step = modify (\s -> s { pos = pos s + step, col = col s + step})

next :: State LAS ()
next = move 1

nextLine :: State LAS ()
nextLine = modify (\s -> s { row = row s + 1, col = 0 })

addToken :: Token -> State LAS ()
addToken token = modify (\s -> s { tokens = tokens s Seq.|> token })

keywords :: [Keyword]
keywords =
  [
    beginKeyword,
    endKeyword,
    programKeyword,
    varKeyword,
    intDivSeq,
    integerKeyword,
    realKeyword
  ]

getCharAt :: Int -> State LAS (Maybe Char)
getCharAt lookAhead = do
  currentState <- get
  let currpos = pos currentState + lookAhead
  if currpos < 0 || currpos > (T.length (text currentState) - 1) then
    return Nothing
  else
    return (Just (T.index (text currentState) currpos))

getCurrChar :: State LAS (Maybe Char)
getCurrChar = do getCharAt 0

peek :: State LAS (Maybe Char)
peek = do getCharAt 1

program1 :: T.Text
program1 = "+ 123+ {TEST} 2.0 \n 1 * 3 PROGRAM myProgram1  1+2 VAR myVar BEGIN END ( ) Var x := 12 ;"

matchRegex :: T.Text -> T.Text -> Bool
matchRegex text regex = T.unpack text =~ regex

numberRegex :: T.Text
numberRegex = "[0-9]+"

plusChar :: T.Text
plusChar = T.pack "+"

minusChar :: T.Text
minusChar = T.pack "-"

divChar :: T.Text
divChar = T.pack "/"

multChar :: T.Text
multChar = T.pack "*"

semiChar :: T.Text
semiChar = T.pack ";"

comaChar :: T.Text
comaChar = T.pack ","

colonChar :: T.Text
colonChar = T.pack ":"

dotChar :: T.Text
dotChar = T.pack "."

lparenChar :: T.Text
lparenChar = T.pack "("

rparenChar :: T.Text
rparenChar = T.pack ")"

intDivSeq :: T.Text
intDivSeq = T.pack "div"

assignSeq :: T.Text
assignSeq = T.pack ":="

containsADot :: T.Text -> Bool
containsADot = T.any (=='.')

matchOperator :: T.Text -> Maybe Token
matchOperator operator
  | operator == plusChar = Just Plus
  | operator == minusChar = Just Minus
  | operator == multChar = Just Mult
  | operator == semiChar = Just Semi
  | operator == divChar = Just Div
  | operator == comaChar = Just Comma
  | operator == lparenChar = Just Lparen
  | operator == rparenChar = Just Rparen
  | operator == colonChar = Just Colon
  | operator == assignSeq = Just Assign
  | operator == dotChar = Just Dot
  | otherwise = Nothing

grabWhile :: T.Text -> Int -> (Char -> T.Text -> Bool) -> T.Text -> T.Text
grabWhile program pos predicate currText 
  | pos >= T.length program = currText
  | predicate char currText = grabWhile program (pos + 1) predicate (T.snoc currText char)
  | otherwise = currText
  where
    char  = T.index program pos

numberCondition :: Char -> T.Text -> Bool
numberCondition currChar totalString = isDigit currChar || (currChar == '.' && not (containsADot totalString))

grabNumber :: T.Text -> Int -> T.Text
grabNumber program start = grabWhile program start numberCondition ""

alphaNumCondition :: Char -> T.Text -> Bool
alphaNumCondition currChar totalString = isAlphaNum currChar

grabAlphanumeric :: T.Text -> Int -> T.Text
grabAlphanumeric program start = grabWhile program start alphaNumCondition ""

isKeyword :: T.Text -> Bool
isKeyword keyword = keyword `elem` keywords

skipWhiteSpaces :: State LAS ()
skipWhiteSpaces = do
  next
  char <- getCurrChar
  case char of
    Nothing -> return ()
    Just a
      | a == ' ' -> skipWhiteSpaces
      | otherwise -> return ()

skipComment :: State LAS ()
skipComment = do
  next
  char <- getCurrChar
  case char of
    Nothing -> return ()
    Just a
      | a /= '}' -> skipComment
      | otherwise -> next

createKeywordToken :: Keyword -> State LAS ()
createKeywordToken keyword
 | keyword == programKeyword = addToken Program
 | keyword == varKeyword = addToken Var
 | keyword == beginKeyword = addToken Begin
 | keyword == endKeyword = addToken End
 | keyword == intDivSeq = addToken IntDiv
 | keyword == realKeyword = addToken Real
 | keyword == integerKeyword = addToken Integer

addErrorToken :: String -> State LAS ()
addErrorToken message = do
  currentState <- get
  addToken (Error (T.pack ("Parsing failed at line "
    ++ show (row currentState + 1) ++ " and col " ++ show (col currentState + 1) ++ ": " ++ message )))

handleNumber :: LAS -> State LAS ()
handleNumber state = do
    let numberText = grabNumber (text state) (pos state)
    move (T.length numberText)
    if containsADot numberText then do
      addToken (FloatVal (read $ T.unpack numberText)) 
    else do
      addToken (IntegerVal (read $ T.unpack numberText))

handleAlphaNum :: LAS -> State LAS ()
handleAlphaNum state = do
    let alphaNumText = grabAlphanumeric (text state) (pos state)
    move (T.length alphaNumText)
    if not (matchRegex alphaNumText "\\<[a-zA-Z]+[0-9]*\\>") then do
      addErrorToken ("Invalid identifier: " ++ T.unpack alphaNumText)
    else do
      case alphaNumText of
        x | isKeyword (T.toLower x) -> do createKeywordToken (T.toLower alphaNumText)
        _ -> do addToken (Id alphaNumText)
      lexer

getOperator :: Char -> State LAS T.Text
getOperator char = do
  if char /= ':' then return (T.pack [char])
  else do
    mNextChar <- peek
    case mNextChar of
      Nothing -> return (T.pack [char])
      Just nextChar -> do
        if nextChar == '=' then return (T.pack [char, nextChar])
        else return (T.pack [char])

handleOtherChar :: Char -> State LAS ()
handleOtherChar char = do
  operator <- getOperator char
  case matchOperator operator of
    Nothing -> addErrorToken ("Unknown character: " ++ [char])
    Just validToken -> do addToken validToken >> move (T.length operator) >> lexer
      
lexer :: State LAS ()
lexer = do
  currentState <- get
  char <- getCurrChar
  case char of
    Nothing -> addToken EOF
    Just a
      | a == ' ' || a == '\r' -> skipWhiteSpaces >> lexer
      | a == '\n' -> next >> nextLine >> lexer
      | a == '{' -> skipComment >> lexer
      | isDigit a -> handleNumber currentState >> lexer
      | isLetter a -> handleAlphaNum currentState
      | otherwise -> handleOtherChar a

run :: String -> IO (Seq.Seq Token)
run fileName = do
    programFile <- TIO.readFile fileName
    let initialState = LAS {
      text = programFile,
      pos = 0,
      tokens = Seq.empty,
      row = 0,
      col = 0
    }
    let (res, state) = runState lexer initialState
    return (tokens state)