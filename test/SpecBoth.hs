{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpecBoth where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import qualified Hedgehog            as H
import           System.Exit
import           System.IO

import qualified Eval
import qualified EvalS
import           Generator
import           JS

eval :: Exp -> Val
eval e = fst $ Eval.eval e []

evalS :: Exp -> Val
evalS e = evalState (EvalS.evalS e) []

newtype TVal = T Val

instance Show TVal where
  show (T v) = show v

-- override equality for NaN
instance Eq TVal where
  T (VNum n) == T (VNum m) | isNaN n = isNaN m
  T v1 == T v2 = v1 == v2

implementationsAgree :: H.Group
implementationsAgree = H.Group "tests for both eval and evalS"
  [ ( "evalS agrees with eval"
    , H.withTests 10000 $ H.property $ do
        e <- H.forAll genExp
        H.annotate $ pretty e
        r :: Either SomeException Val <- liftIO $ try $ evaluate $ force $ eval e
        H.annotateShow r
        rS :: Either SomeException Val <- liftIO $ try $ evaluate $ force $ evalS e
        H.annotateShow rS
        case (r,rS) of
          (Left _, Left _) -> H.success
          (Right v, Right vS) -> T v H.=== T vS
          _ -> H.failure)
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  r <- H.checkSequential implementationsAgree
  unless r $ exitFailure
