module Spec where

import           Control.Monad
import           Hedgehog      (Gen, Property, check, evalIO, forAll,
                                property, (===))

import           Eval          (eval)
import           Generator
import           JS
import           Run

agreesWithNode :: Gen Exp -> Property
agreesWithNode gen = property $ do
  e <- forAll gen
  Right vN <- evalIO $ runInNode e
  let (v, _) = eval e []
  v === vN

main :: IO ()
main = do
  void $ check $ agreesWithNode genSeqArith
  void $ check $ agreesWithNode genExp
