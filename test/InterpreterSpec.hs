module InterpreterSpec where

import Test.HUnit
import Data.Map as Map
import Data.Text as T

import Lexer
import Parser
import Interpreter

runFullPipeline :: Lexer.RawProgram -> Interpreter.Memory
runFullPipeline programString = do
     let tokens = Lexer.run programString
     let mProgram = Parser.run tokens
     case mProgram of 
        Nothing -> Map.empty 
        Just program -> do Interpreter.run program

hasCorrectVarVal :: Interpreter.Memory -> String -> Interpreter.PascalValue -> Bool
hasCorrectVarVal mem varName val = do
    let mVal = Map.lookup (T.pack varName) mem 
    case mVal of
        Nothing -> False
        Just storedVal -> do
            case (val, storedVal) of
                (IntVal a, IntVal b) -> a == b
                (Interpreter.FloatVal a, Interpreter.FloatVal b) -> a == b
                _ -> False

testProgram1 :: Test
testProgram1 = TestLabel "BEGIN BEGIN a := 5; b := a + 2 END; END." $ TestCase $ do
    let rawProgram = T.pack "BEGIN BEGIN a := 5; b := a + 2 END; END."
    let memory = runFullPipeline rawProgram
    assertBool "variable a is set to 5" (hasCorrectVarVal memory "a" (Interpreter.IntVal 5))
    assertBool "variable b is set to 7" (hasCorrectVarVal memory "a" (Interpreter.IntVal 7))

myTestSuite :: Test
myTestSuite = test [ testProgram1 ]

runTests :: IO ()
runTests = do
    counts <- runTestTT myTestSuite
    if failures counts == 0
        then putStrLn "All tests passed!"
        else putStrLn "Some tests failed"
