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

genNumVal :: MonadGen m => m Val
genNumVal = VNum <$> Gen.frequency
  [ (3, (* 1000) <$> Gen.double (Range.exponentialFloat (-1) 1))
  , (7, fromIntegral <$> Gen.integral (Range.linear (-10000) 10000))
  ]

genBoolVal :: MonadGen m => m Val
genBoolVal = VBool <$> Gen.bool

genVal :: MonadGen m => m Val
genVal = Gen.frequency
  [ (15, genNumVal)
  , (10, genBoolVal)
  , (1, return VUndefined)
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

genLeaf :: MonadGen m => Set String -> m Val -> m Exp
genLeaf vs vg
  | Set.null vs = Lit <$> vg
  | otherwise   = Gen.choice [ Lit <$> vg, Var <$> Gen.element (Set.toList vs)]

genExp' :: MonadGen m => StateT (Set String) m Exp
genExp' = do
  vs <- get
  Gen.recursive Gen.choice
    [ genLeaf vs genVal ]
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

genBadExp :: MonadGen m => m Exp
genBadExp = evalStateT genBadExp' Set.empty

genBadExp' :: MonadGen m => StateT (Set String) m Exp
genBadExp' = do
  vs <- get
  Gen.recursive Gen.choice
    [ return $ Var "bad_var" ]
    [ Unary <$> Gen.enumBounded <*> genBadExp'
    , do
        op <- Gen.enumBounded
        e1 <- genBadExp'
        old <- get
        e2 <- genExp'
        when (isShortCircuit op) $ put old
        return $ Bin op e1 e2
    , do
        op <- Gen.enumBounded
        e1 <- genExp'
        old <- get
        e2 <- genBadExp'
        when (isShortCircuit op) $ put old
        return $ Bin op e1 e2
    , do
        c <- genBadExp'
        old <- get
        e1 <- genExp'
        put old
        e2 <- genExp'
        put old
        return $ Cond c e1 e2
    , do
        c <- genExp'
        old <- get
        e1 <- genBadExp'
        put old
        e2 <- genExp'
        put old
        return $ Cond c e1 e2
    , do
        c <- genExp'
        old <- get
        e1 <- genExp'
        put old
        e2 <- genBadExp'
        put old
        return $ Cond c e1 e2
    , do
        v <- genVarName
        e <- genBadExp'
        put $ Set.insert v vs
        return $ Assign v e
    , Seq <$> genBadExp' <*> genExp'
    , Seq <$> genExp' <*> genBadExp'
    ]

-- same as Arith but without Mod
genArithExpNoMod :: MonadGen m => Set String -> m Exp
genArithExpNoMod vs = Gen.recursive Gen.choice
  [ genLeaf vs genNumVal ]
  [ Gen.subterm2 (genArithExpNoMod vs) (genArithExpNoMod vs) (Bin Add)
  , Gen.subterm2 (genArithExpNoMod vs) (genArithExpNoMod vs) (Bin Sub)
  , Gen.subterm2 (genArithExpNoMod vs) (genArithExpNoMod vs) (Bin Mul)
  , Gen.subterm2 (genArithExpNoMod vs) (genArithExpNoMod vs) (Bin Div)
  ]

genArithExpNoDiv :: MonadGen m => Set String -> m Exp
genArithExpNoDiv vs = Gen.recursive Gen.choice
  [ genLeaf vs genNumVal ]
  [ Gen.subterm2 (genArithExpNoDiv vs) (genArithExpNoDiv vs) (Bin Add)
  , Gen.subterm2 (genArithExpNoDiv vs) (genArithExpNoDiv vs) (Bin Sub)
  , Gen.subterm2 (genArithExpNoDiv vs) (genArithExpNoDiv vs) (Bin Mul)
  ]

genArithExp :: MonadGen m => Set String -> m Exp
genArithExp vs = Gen.recursive Gen.choice
  [ genLeaf vs genNumVal ]
  [ Gen.subterm2 (genArithExp vs) (genArithExp vs) (Bin Add)
  , Gen.subterm2 (genArithExp vs) (genArithExp vs) (Bin Sub)
  , Gen.subterm2 (genArithExp vs) (genArithExp vs) (Bin Mul)
  , Gen.subterm2 (genArithExp vs) (genArithExp vs) (Bin Div)
  , Gen.subterm2 (genArithExp vs) (genArithExp vs) (Bin Mod)
  ]

genSeqExp' :: MonadGen m => Set String -> (Set String -> m Exp) -> m Exp
genSeqExp' vs base = Gen.recursive Gen.choice
  [ base vs ]
  [ do
      v <- genVarName
      e <- base vs
      rest <- genSeqExp' (Set.insert v vs) base
      return $ Seq (Assign v e) rest
  ]

genSeqArith :: MonadGen m => m Exp
genSeqArith = genSeqExp' Set.empty genArithExp

genSeqArithNoMod :: MonadGen m => m Exp
genSeqArithNoMod = genSeqExp' Set.empty genArithExpNoMod

genSeqArithNoDiv :: MonadGen m => m Exp
genSeqArithNoDiv = genSeqExp' Set.empty genArithExpNoDiv
