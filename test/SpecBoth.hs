{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpecBoth where

import           Control.DeepSeq
import           Control.Exception
import           Control.Monad
import           Control.Monad.State
import qualified Hedgehog            as H
import           System.Exit

import qualified Eval
import qualified EvalS
import           Generator
import           JS

eval :: Exp -> Val
eval e = fst $ Eval.eval e []

evalS :: Exp -> Val
evalS e = evalState (EvalS.evalS e) []

implementationsAgree :: H.Group
implementationsAgree = H.Group "tests for both eval and evalS"
  [ ( "evalS agrees with eval"
    , H.withTests 1000 $ H.property $ do
        e <- H.forAll genExp
        H.annotate $ pretty e
        r :: Either SomeException Val <- liftIO $ try $ evaluate $ force $ eval e
        H.annotateShow r
        rS :: Either SomeException Val <- liftIO $ try $ evaluate $ force $ evalS e
        H.annotateShow rS
        case (r,rS) of
          (Left _, Left _) -> H.success
          (Right v, Right vS) -> v H.=== vS
          _ -> H.failure)
  ]

main :: IO ()
main = do
  r <- H.checkSequential implementationsAgree
  unless r $ exitFailure
