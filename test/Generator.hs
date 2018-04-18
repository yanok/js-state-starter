{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Generator where

import           Control.Monad.State
import           Data.Set            (Set)
import qualified Data.Set            as Set
import           Hedgehog            (MonadGen)
import qualified Hedgehog.Gen        as Gen
import qualified Hedgehog.Range      as Range

import           JS

genVal :: MonadGen m => m Val
genVal = Gen.frequency
  [ (3, VNum <$> (* 100) <$> Gen.double (Range.exponentialFloat (-100) 100))
  , (7, VNum <$> fromIntegral <$> Gen.integral (Range.linear (-10000) 10000))
  , (5, VBool <$> Gen.bool)
  , (2, return VUndefined)
  , (1, return $ VNum (0/0)) -- NaN
  , (1, return $ VNum (1/0)) -- +Infinity
  , (1, return $ VNum (-1/0)) -- -Infinity
  ]

genVarName :: MonadGen m => m String
genVarName = Gen.string (Range.linear 1 10) Gen.alpha

genExp :: MonadGen m => m Exp
genExp = evalStateT genExp' Set.empty

isShortCircuit :: Op -> Bool
isShortCircuit And = True
isShortCircuit Or = True
isShortCircuit _ = False

genExp' :: MonadGen m => StateT (Set String) m Exp
genExp' = do
  vs <- get
  Gen.recursive Gen.choice
    (if Set.null vs
     then [ Lit <$> genVal ]
     else [ Lit <$> genVal
          , Var <$> Gen.element (Set.toList vs)])
    [ Unary <$> Gen.enumBounded <*> genExp'
    , do
        op <- Gen.enumBounded
        e1 <- genExp'
        old <- get
        e2 <- genExp'
        when (isShortCircuit op) $ put old
        return $ Bin op e1 e2
    , do
        c <- genExp'
        old <- get
        e1 <- genExp'
        put old
        e2 <- genExp'
        put old
        return $ Cond c e1 e2
    , do
        v <- genVarName
        e <- genExp'
        put $ Set.insert v vs
        return $ Assign v e
    , Seq <$> genExp' <*> genExp'
    ]
