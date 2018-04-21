module SpecEvalS where

import           Control.Monad
import           Control.Monad.State
import qualified Hedgehog            as H
import           System.Exit

import qualified EvalS
import           JS
import           Spec

evalS :: Exp -> Val
evalS e = evalState (EvalS.evalS e) []

main :: IO ()
main = do
  rs <- traverse H.checkSequential $ testsForImplementation evalS
  unless (all id rs) $ exitFailure
