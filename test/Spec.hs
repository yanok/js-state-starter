{-# LANGUAGE OverloadedStrings #-}
module Spec where

import           Control.Monad
import           Data.String
import           Hedgehog      (Gen, Group (..), Property, check,
                                checkSequential, evalIO, forAll, property,
                                (===))

import qualified Eval
import           Generator
import           JS
import           Run

eval :: Exp -> Val
eval e = fst $ Eval.eval e []

agreesWithNode :: (Exp -> Val) -> Gen Exp -> Property
agreesWithNode ev gen = property $ do
  e <- forAll gen
  Right vN <- evalIO $ runInNode e
  ev e === vN

testsForImplementation :: String -> (Exp -> Val) -> Group
testsForImplementation name ev = Group (fromString name)
  [ ("arith-only expression with no inline assignment", agreesWithNode ev genSeqArith)
  , ( "simple arith expression with no inline assignment"
    , agreesWithNode ev genSeqSimpleArith)
  , ("general expressions", agreesWithNode ev genExp)
  ]

main :: IO ()
main = do
  void $ checkSequential $ testsForImplementation "eval" eval
