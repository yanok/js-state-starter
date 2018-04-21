{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module SpecEvalS where

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
import           Spec

eval :: Exp -> Val
eval e = fst $ Eval.eval e []

evalS :: Exp -> Val
evalS e = evalState (EvalS.evalS e) []

implementationsAgree :: H.Group
implementationsAgree = H.Group "extra test for evalS"
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
  rs <- traverse H.checkSequential (implementationsAgree:testsForImplementation evalS)
  unless (all id rs) $ exitFailure
