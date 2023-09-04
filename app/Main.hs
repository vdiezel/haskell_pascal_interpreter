{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- Writing this rather imperatively for performance reasons

module Main where

import Control.Monad.State
import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Regex.TDFA
import Data.Char
import qualified Data.Sequence as Seq
import Debug.Trace
import Data.Maybe
import Lexer

main :: IO ()
main = do
    tokens <- Lexer.run "./programs/program1.pas"
    print (show tokens)