{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Data.String
import           Hedgehog            (Gen, Group (..), Property, check,
                                      checkSequential, evalEither, evalIO,
                                      failure, forAll, property, success, (===))
import qualified Hedgehog            as H

import qualified Eval
import qualified EvalS
import           Generator
import           JS
import           Run

eval :: Exp -> Val
eval e = fst $ Eval.eval e []

evalS :: Exp -> Val
evalS e = evalState (EvalS.evalS e) []

agreesWithNode :: (Exp -> Val) -> Gen Exp -> Property
agreesWithNode ev gen = property $ do
  e <- forAll gen
  rN <- evalIO $ runInNode e
  vN <- evalEither rN
  v <- H.eval $ ev e
  v === vN

detectsBadVar :: (Exp -> Val) -> Gen Exp -> Property
detectsBadVar ev gen = property $ do
  e <- forAll gen
  r <- liftIO $ try $ evaluate $ force $ ev e
  case r :: Either SomeException Val of
    Left _ -> success
    Right _ -> failure

testsForImplementation :: String -> (Exp -> Val) -> Group
testsForImplementation name ev = Group (fromString name)
  [ ( "simple arith expression (no division) with no inline assignment"
    , agreesWithNode ev genSeqArithNoDiv)
  , ( "simple arith expression (no modulo) with no inline assignment"
    , agreesWithNode ev genSeqArithNoMod)
  , ("arith-only expression with no inline assignment", agreesWithNode ev genSeqArith)
  , ("general expressions", agreesWithNode ev genExp)
  , ( "detects used undefined variable in arith (no division)"
    , detectsBadVar ev genSeqBadArithNoDiv)
  , ("detects used undefined variable in arith", detectsBadVar ev genSeqBadArith)
  , ("detects used undefined variable", detectsBadVar ev genSeqBadVar)
  , ("detects undefined variable (even unused)", detectsBadVar ev genBadExp)
  ]

main :: IO ()
main = do
  void $ checkSequential $ testsForImplementation "eval" eval
  void $ checkSequential $ testsForImplementation "evalS" evalS
