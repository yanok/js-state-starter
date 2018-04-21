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
genVarName = ("var" ++) <$> Gen.string (Range.linear 1 10) Gen.alpha

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

-- avoid common problems: modulo and funny values
genSafeExp :: MonadGen m => m Exp
genSafeExp = evalStateT genSafeExp' Set.empty

genSafeExp' :: MonadGen m => StateT (Set String) m Exp
genSafeExp' = do
  vs <- get
  Gen.recursive Gen.choice
    [ genLeaf vs (Gen.choice [genNumVal, genBoolVal]) ]
    [ Unary <$> Gen.enumBounded <*> genSafeExp'
    , do
        op <- Gen.filter (/= Mod) Gen.enumBounded
        e1 <- genSafeExp'
        old <- get
        e2 <- genSafeExp'
        when (isShortCircuit op) $ put old
        return $ Bin op e1 e2
    , do
        c <- genSafeExp'
        old <- get
        e1 <- genSafeExp'
        put old
        e2 <- genSafeExp'
        put old
        return $ Cond c e1 e2
    , do
        v <- genVarName
        e <- genSafeExp'
        put $ Set.insert v vs
        return $ Assign v e
    , Seq <$> genSafeExp' <*> genSafeExp'
    ]

genExpNoDiv :: MonadGen m => m Exp
genExpNoDiv = evalStateT genExpNoDiv' Set.empty

genExpNoDiv' :: MonadGen m => StateT (Set String) m Exp
genExpNoDiv' = do
  vs <- get
  Gen.recursive Gen.choice
    [ genLeaf vs genNumVal ]
    [ Bin Add <$> genExpNoDiv' <*> genExpNoDiv'
    , Bin Sub <$> genExpNoDiv' <*> genExpNoDiv'
    , Bin Mul <$> genExpNoDiv' <*> genExpNoDiv'
    , do
        v <- genVarName
        e <- genExpNoDiv'
        put $ Set.insert v vs
        return $ Assign v e
    , Seq <$> genExpNoDiv' <*> genExpNoDiv'
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
    -- , do
    --     op <- Gen.enumBounded
    --     e1 <- genExp'
    --     old <- get
    --     e2 <- genBadExp'
    --     when (isShortCircuit op) $ put old
    --     return $ Bin op e1 e2
    , do
        c <- genBadExp'
        old <- get
        e1 <- genExp'
        put old
        e2 <- genExp'
        put old
        return $ Cond c e1 e2
    -- , do
    --     c <- genExp'
    --     old <- get
    --     e1 <- genBadExp'
    --     put old
    --     e2 <- genExp'
    --     put old
    --     return $ Cond c e1 e2
    -- , do
    --     c <- genExp'
    --     old <- get
    --     e1 <- genExp'
    --     put old
    --     e2 <- genBadExp'
    --     put old
    --     return $ Cond c e1 e2
    , do
        v <- genVarName
        e <- genBadExp'
        put $ Set.insert v vs
        return $ Assign v e
    , Seq <$> genBadExp' <*> genExp'
    , Seq <$> genExp' <*> genBadExp'
    ]

-- avoid Mod, Div, SEq and SNe
genSafeSimpleBadExp :: MonadGen m => String -> Set String -> m Exp
genSafeSimpleBadExp bad vs = do
  Gen.recursive Gen.choice
    [ return $ Var bad ]
    [ Unary <$> Gen.enumBounded <*> genSimpleBadExp bad vs
    , do
        op <- Gen.filter (not . (`elem` [Mod, Div, SEq, SNe])) Gen.enumBounded
        e1 <- genSafeSimpleBadExp bad vs
        e2 <- genArithExp vs
        return $ Bin op e1 e2
    , do
        c <- genSafeSimpleBadExp bad vs
        e1 <- genArithExp vs
        e2 <- genArithExp vs
        return $ Cond c e1 e2
    ]
genSimpleBadExp :: MonadGen m => String -> Set String -> m Exp
genSimpleBadExp bad vs = do
  Gen.recursive Gen.choice
    [ return $ Var bad ]
    [ Unary <$> Gen.enumBounded <*> genSimpleBadExp bad vs
    , do
        op <- Gen.enumBounded
        e1 <- genSimpleBadExp bad vs
        e2 <- genArithExp vs
        return $ Bin op e1 e2
    , do
        c <- genSimpleBadExp bad vs
        e1 <- genArithExp vs
        e2 <- genArithExp vs
        return $ Cond c e1 e2
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

genArithBadExp :: MonadGen m => String -> Set String -> m Exp
genArithBadExp v vs = Gen.recursive Gen.choice
  [ pure $ Var v ]
  [ Gen.subtermM (genArithBadExp v vs) (\x -> Bin Add <$> genArithExp vs <*> pure x)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Add x <$> genArithExp vs)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Sub <$> genArithExp vs <*> pure x)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Sub x <$> genArithExp vs)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Mul <$> genArithExp vs <*> pure x)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Mul x <$> genArithExp vs)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Div <$> genArithExp vs <*> pure x)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Div x <$> genArithExp vs)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Mod <$> genArithExp vs <*> pure x)
  , Gen.subtermM (genArithBadExp v vs) (\x -> Bin Mod x <$> genArithExp vs)
  ]

genArithBadExpNoDiv :: MonadGen m => String -> Set String -> m Exp
genArithBadExpNoDiv v vs = Gen.recursive Gen.choice
  [ pure $ Var v ]
  [ Gen.subtermM (genArithBadExpNoDiv v vs)
    (\x -> Bin Add <$> genArithExpNoDiv vs <*> pure x)
  , Gen.subtermM (genArithBadExpNoDiv v vs)
    (\x -> Bin Add x <$> genArithExpNoDiv vs)
  , Gen.subtermM (genArithBadExpNoDiv v vs)
    (\x -> Bin Sub <$> genArithExpNoDiv vs <*> pure x)
  , Gen.subtermM (genArithBadExpNoDiv v vs)
    (\x -> Bin Sub x <$> genArithExpNoDiv vs)
  , Gen.subtermM (genArithBadExpNoDiv v vs)
    (\x -> Bin Mul <$> genArithExpNoDiv vs <*> pure x)
  , Gen.subtermM (genArithBadExpNoDiv v vs)
    (\x -> Bin Mul x <$> genArithExpNoDiv vs)
  ]

genSeqExp :: MonadGen m => Set String -> (Set String -> m Exp) -> m Exp
genSeqExp vs base = Gen.recursive Gen.choice
  [ base vs ]
  [ do
      v <- genVarName
      e <- base vs
      rest <- genSeqExp (Set.insert v vs) base
      return $ Seq (Assign v e) rest
  ]

genSeqExpErr
    :: MonadGen m
    => String
    -> Set String
    -> (String -> Set String -> m Exp)
    -> (Set String -> m Exp)
    -> m Exp
genSeqExpErr init vs bad good = Gen.recursive Gen.choice
  [ bad init vs ]
  [ do
      v <- genVarName
      e <- bad init vs
      rest <- genSeqExpErr v vs bad good
      return $ Seq (Assign v e) rest
  , do
      v <- Gen.filter (/= init) genVarName
      e <- good vs
      rest <- genSeqExpErr init (Set.insert v vs) bad good
      return $ Seq (Assign v e) rest
  ]

genSeqArith :: MonadGen m => m Exp
genSeqArith = genSeqExp Set.empty genArithExp

genSeqArithNoMod :: MonadGen m => m Exp
genSeqArithNoMod = genSeqExp Set.empty genArithExpNoMod

genSeqArithNoDiv :: MonadGen m => m Exp
genSeqArithNoDiv = genSeqExp Set.empty genArithExpNoDiv

genSeqBadVar :: MonadGen m => m Exp
genSeqBadVar = genSeqExpErr "bad_var" Set.empty genSimpleBadExp genArithExp

genSeqSafeBad :: MonadGen m => m Exp
genSeqSafeBad = genSeqExpErr "bad_var" Set.empty genSafeSimpleBadExp genArithExp

genSeqBadArith :: MonadGen m => m Exp
genSeqBadArith = genSeqExpErr "bad_var" Set.empty genArithBadExp genArithExp

genSeqBadArithNoDiv :: MonadGen m => m Exp
genSeqBadArithNoDiv
  = genSeqExpErr "bad_var" Set.empty genArithBadExpNoDiv genArithExpNoDiv

genFalsy :: MonadGen m => m Exp
genFalsy = Gen.recursive Gen.choice
  [ pure $ Lit $ VBool False
  , pure $ Lit $ VNum 0
  ]
  [ Bin And <$> genFalsy <*> genArithExp Set.empty
  , Bin And <$> genArithExp Set.empty <*> genFalsy
  , Gen.subterm2 genFalsy genFalsy (Bin Or)
  ]

genTruthy :: MonadGen m => m Exp
genTruthy = Gen.recursive Gen.choice
  [ pure $ Lit $ VBool True
  , pure $ Lit $ VNum 1
  ]
  [ Bin Or <$> genTruthy <*> genArithExp Set.empty
  , Bin Or <$> genArithExp Set.empty <*> genTruthy
  , Gen.subterm2 genTruthy genTruthy (Bin And)
  ]

genSCAnd :: MonadGen m => m Exp
genSCAnd = do
  v <- genVarName
  e <- genExp
  c <- genFalsy
  e2 <- genSafeSimpleBadExp v Set.empty
  return $ Seq (Bin And c (Assign v e)) e2

genSCOr :: MonadGen m => m Exp
genSCOr = do
  v <- genVarName
  e <- genExp
  c <- genTruthy
  e2 <- genSafeSimpleBadExp v Set.empty
  return $ Seq (Bin Or c (Assign v e)) e2

genSCCondT :: MonadGen m => m Exp
genSCCondT = do
  v <- genVarName
  e <- genExp
  c <- genTruthy
  e2 <- genArithExp Set.empty
  e3 <- genSafeSimpleBadExp v Set.empty
  return $ Seq (Cond c e2 (Assign v e)) e3

genSCCondF :: MonadGen m => m Exp
genSCCondF = do
  v <- genVarName
  e <- genExp
  c <- genFalsy
  e2 <- genArithExp Set.empty
  e3 <- genSafeSimpleBadExp v Set.empty
  return $ Seq (Cond c (Assign v e) e2) e3
