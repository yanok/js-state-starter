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

newtype TVal = T Val

instance Show TVal where
  show (T v) = show v

isCloseEnough :: Double -> Double -> Bool
isCloseEnough x y = abs (x - y) / x < 0.001

instance Eq TVal where
  T (VNum n) == T (VNum m) | isNaN n      = isNaN m
                           | isInfinite n = isInfinite m && signum n == signum m
                           | abs n == 0   = n == m
                           | otherwise    = isCloseEnough n m
  T (VBool b1) == T (VBool b2) = b1 == b2
  T VUndefined == T VUndefined = True
  _ == _ = False

agreesWithNode :: (Exp -> Val) -> Gen Exp -> Property
agreesWithNode ev gen = H.withTests 300 $ property $ do
  e <- forAll gen
  H.annotate $ pretty e
  rN <- evalIO $ runInNode e
  vN <- evalEither rN
  v <- H.eval $ ev e
  T v === T vN

detectsBadVar :: (Exp -> Val) -> Gen Exp -> Property
detectsBadVar ev gen = H.withTests 1000 $ property $ do
  e <- forAll gen
  H.annotate $ pretty e
  r <- evalIO $ try $ evaluate $ force $ ev e
  case r :: Either SomeException Val of
    Left _ -> success
    Right _ -> failure

testsForImplementation :: (Exp -> Val) -> [Group]
testsForImplementation ev =
  [ Group "basic tests for (over)simplified expressions"
    [ ( "simple arith expression (no division) with no inline assignment"
      , agreesWithNode ev genSeqArithNoDiv)
    , ( "simple arith expression (no modulo) with no inline assignment"
      , agreesWithNode ev genSeqArithNoMod)
    , ( "detects used undefined variable in arith (no division)"
      , detectsBadVar ev genSeqBadArithNoDiv)
    , ( "detects used undefined variable in 'safe' (no lazy operations) expressions"
      , detectsBadVar ev genSeqSafeBad)
    , ( "assignment inside assignment"
      , agreesWithNode ev genAssignAssign)
    , ( "assignment inside binary operator"
      , agreesWithNode ev $ genBinopAssign (/= Mod))
    , ( "arith (no division) with inline assignment"
      , agreesWithNode ev genExpNoDiv)
    , ( "'safe' (no mod and funny values) expressions"
      , agreesWithNode ev genSafeExp)
    ]
  , Group "short circuiting operations"
    [ ( "and false"
      , detectsBadVar ev genSCAnd)
    , ( "or true"
      , detectsBadVar ev genSCOr)
    , ( "cond true"
      , detectsBadVar ev genSCCondT)
    , ( "cond false"
      , detectsBadVar ev genSCCondF)
    ]
  , Group "trickier expressions (mostly tests previous assignment)"
    [ ( "arith-only expression with no inline assignment"
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
