module Spec where

import           Control.Monad
import           Hedgehog      (check, evalIO, forAll, property, (===))

import           Eval          (eval)
import           Generator
import           JS
import           Run


main :: IO ()
main = void $ check $ property $ do
  e <- forAll genExp
  Right vN <- evalIO $ runInNode e
  let (v, _) = eval e []
  v === vN
