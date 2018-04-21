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

import           Generator
import           JS
import           Run

agreesWithNode :: (Exp -> Val) -> Gen Exp -> Property
agreesWithNode ev gen = property $ do
  e <- forAll gen
  H.annotate $ pretty e
  rN <- evalIO $ runInNode e
  vN <- evalEither rN
  v <- H.eval $ ev e
  v === vN

detectsBadVar :: (Exp -> Val) -> Gen Exp -> Property
detectsBadVar ev gen = property $ do
  e <- forAll gen
  H.annotate $ pretty e
  r <- evalIO $ try $ evaluate $ force $ ev e
  case r :: Either SomeException Val of
    Left _ -> success
    Right _ -> failure

testsForImplementation :: (Exp -> Val) -> [Group]
testsForImplementation ev =
  [ Group "basic tests for over-simplified expressions"
    [ ( "simple arith expression (no division) with no inline assignment"
      , agreesWithNode ev genSeqArithNoDiv)
    , ( "detects used undefined variable in arith (no division)"
      , detectsBadVar ev genSeqBadArithNoDiv)
    , ( "arith (no division) with inline assignment"
      , agreesWithNode ev genExpNoDiv)
    ]
  , Group "trickier expressions (mostly tests previous assignment)"
    [ ( "simple arith expression (no modulo) with no inline assignment"
      , agreesWithNode ev genSeqArithNoMod)
    , ( "arith-only expression with no inline assignment"
      , agreesWithNode ev genSeqArith)
    , ( "general expressions"
      , agreesWithNode ev genExp)
    ]
  , Group "trickier error detection"
    [ ( "detects used undefined variable in arith"
      , detectsBadVar ev genSeqBadArith)
    , ( "detects used undefined variable"
      , detectsBadVar ev genSeqBadVar)
    , ( "detects undefined variable (even unused)"
      , detectsBadVar ev genBadExp)
    ]
  ]
