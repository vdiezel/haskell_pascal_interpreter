{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Text.IO as TIO
import Lexer
import Parser
import Interpreter

main :: IO ()
main = do
     programFile <- TIO.readFile "./programs/program4.pas"
     let tokens = Lexer.run programFile
     print (show tokens)
     let mProgram = Parser.run tokens
     case mProgram of
          Nothing -> print "Parsing failed"
          Just program -> do
               let res = Interpreter.run program
               print (show res)