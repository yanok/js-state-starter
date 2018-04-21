module EvalS where

import System.Console.Haskeline
import System.Environment (getArgs)
import Control.Monad.State
import Data.Fixed (mod')
import JS

------------------------------------------------------------------------
-- MAKE YOUR CHANGES HERE
------------------------------------------------------------------------

-- evaluate an expression
evalS :: Exp -> State Store Val
evalS (Lit v) = return v
evalS e = error ("evaluation failed for " ++ show e)

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
        putStrLn $ "evalS -> " ++ prettyVal (evalState (evalS e) [])
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
          let (v, store') = runState (evalS e) store
          outputStrLn $ "evalS -> " ++ prettyVal v
          readEvalPrintLoop store'
        Left err -> do
          outputStrLn $ show err
          readEvalPrintLoop store

