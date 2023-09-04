{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- TODO
-- add variables
-- add keywords support

-- Writing this rather imperatively for performance reasons

module Main where

import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import Text.Regex.TDFA
import Data.Char
import qualified Data.Sequence as Seq
import Debug.Trace

type RawProgram = T.Text
type Keyword = T.Text


varKeyword :: Keyword

programKeyword = "PROGRAM" :: Keyword
beginKeyword = "BEGIN" :: Keyword
varKeyword = "VAR" :: Keyword
endKeyword = "END" :: Keyword

data Token = IntegerVal Int
  | FloatVal Float
  | Plus
  | Mult
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
    varKeyword
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

peak :: State LAS (Maybe Char)
peak = do getCharAt 1

program1 :: T.Text
program1 = "+ 123+ 2.0 \n 1 * 3 PROGRAM myProgram1  1+2 VAR myVar BEGIN END"

matchRegex :: T.Text -> T.Text -> Bool
matchRegex text regex = T.unpack text =~ regex

numberRegex :: T.Text
numberRegex = "[0-9]+"

plusChar :: T.Text
plusChar = T.pack "+"

multChar :: T.Text
multChar = T.pack "*"

containsADot :: T.Text -> Bool
containsADot = T.any (=='.')

matchSingleChar :: T.Text -> Maybe Token
matchSingleChar char
  | char == plusChar = Just Plus
  | char == multChar = Just Mult
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

createKeywordToken :: Keyword -> State LAS ()
createKeywordToken keyword
 | keyword == programKeyword = addToken Program
 | keyword == varKeyword = addToken Var
 | keyword == beginKeyword = addToken Begin
 | keyword == endKeyword = addToken End

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
      addErrorToken "Invalid identifier"
    else do
      case alphaNumText of
        x | isKeyword x -> do createKeywordToken alphaNumText
        _ -> do addToken (Id alphaNumText)

handleOtherChar :: Char -> State LAS ()
handleOtherChar char = do
  case matchSingleChar (T.pack [char]) of
    Nothing -> addErrorToken "Unknown character"
    Just validToken -> do
      addToken validToken >> next >> lexer

lexer :: State LAS ()
lexer = do
  currentState <- get
  char <- getCurrChar
  case char of
    Nothing -> addToken EOF
    Just a
      | a == ' ' || a == '\r' -> skipWhiteSpaces >> lexer
      | a == '\n' -> next >> nextLine >> lexer
      | isDigit a -> handleNumber currentState >> lexer
      | isLetter a -> handleAlphaNum currentState >> lexer
      | otherwise -> handleOtherChar a

main :: IO ()
main = do
    let initialState = LAS {
      text = program1,
      pos = 0,
      tokens = Seq.empty,
      row = 0,
      col = 0
    }
    let (res, state) = runState lexer initialState
    print (show (tokens state))