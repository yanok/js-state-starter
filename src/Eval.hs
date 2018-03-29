module Eval where

import System.Console.Haskeline
import System.Environment (getArgs)
import Data.Fixed (mod')
import JS

------------------------------------------------------------------------
-- MAKE YOUR CHANGES HERE
------------------------------------------------------------------------

-- evaluate an expression
eval :: Exp -> Store -> (Val, Store)
eval (Lit v) s = (v, s)
eval e s = error ("evaluation failed for " ++ show e)

------------------------------------------------------------------------
-- DO NOT CHANGE THE CODE BELOW
------------------------------------------------------------------------

-- main and the REPL
main :: IO ()
main = do
  args <- getArgs
  if null args then
    runInputT defaultSettings (readEvalPrintLoop [])
  else do
    input <- readFile $ head args
    case jsExp input of
      Right e -> do
        putStrLn $ "eval --> " ++ show (fst (eval e []))
      Left err ->
        putStrLn $ show err

readEvalPrintLoop :: Store -> InputT IO ()
readEvalPrintLoop store = do
  maybeLine <- getInputLine "> "
  case maybeLine of
    Nothing -> return ()
    Just line ->
      case command line of
        Right Quit -> return ()
        Right (Eval e)  -> do
          let (v, s) = eval e store
          outputStrLn $ "eval --> " ++ show v
          readEvalPrintLoop s
        Left err -> do
          outputStrLn $ show err
          readEvalPrintLoop store

