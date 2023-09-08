{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text.IO as TIO
import Lexer
import Parser
import Interpreter

main :: IO ()
main = do
     programFile <- TIO.readFile "./programs/program3.pas"
     tokens <- Lexer.run programFile
     mProgram <- Parser.run tokens
     case mProgram of
          Nothing -> print "Parsing failed"
          Just program -> do
               execState <- Interpreter.run program
               print (show execState)