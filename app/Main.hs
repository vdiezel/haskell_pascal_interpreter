{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Writing this rather imperatively for performance reasons

module Main where

import Lexer

main :: IO ()
main = do
    tokens <- Lexer.run "./programs/program1.pas"
    print (show tokens)