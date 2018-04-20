{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import           Data.String
import           Hedgehog            (Gen, Group (..), Property, check,
                                      checkSequential, evalIO, failure, forAll,
                                      property, success, (===))

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
  Right vN <- evalIO $ runInNode e
  ev e === vN

detectsBadVar :: (Exp -> Val) -> Property
detectsBadVar ev = property $ do
  e <- forAll genBadExp
  r <- liftIO $ try $ evaluate $ ev e
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
  , ("detects undefined variable", detectsBadVar ev)
  ]

main :: IO ()
main = do
  void $ checkSequential $ testsForImplementation "eval" eval
  void $ checkSequential $ testsForImplementation "evalS" evalS
