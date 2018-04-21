module SpecEval where

import           Control.Monad
import qualified Hedgehog      as H
import           System.Exit
import           System.IO

import qualified Eval
import           JS
import           Spec

eval :: Exp -> Val
eval e = fst $ Eval.eval e []

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  rs <- traverse H.checkSequential $ testsForImplementation eval
  unless (all id rs) $ exitFailure
