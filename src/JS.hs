{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
module JS where

import           Control.Applicative               ((<$>), (<*>))
import           Control.DeepSeq
import           GHC.Generics
import           Language.JavaScript.Parser.AST
import           Language.JavaScript.Parser.Parser

data Op = Add | Sub | Mul | Div | Mod | Gt | Lt | Eq | Le | Ge | And | Or | SEq | Ne | SNe
  deriving (Show, Eq, Enum, Bounded)

data UOp = Plus | Minus | Not deriving (Show, Eq, Enum, Bounded)

data Exp = Unary UOp Exp         {- A unary operation -}
         | Bin Op Exp Exp        {- A binary operation -}
         | Cond Exp Exp Exp      {- cond ? yes : no -}
         | Lit Val               {- literal values -}
         | Var String            {- variables -}
         | Assign String Exp     {- assignment -}
         | Seq Exp Exp           {- e1; e2 -}
         deriving Show

data Val = VNum Double | VBool Bool | VUndefined
  deriving (Generic, NFData, Show)

isCloseEnough :: Double -> Double -> Bool
isCloseEnough x y = abs (x - y) / x < 0.001

instance Eq Val where
  VNum n == VNum m | isNaN n      = isNaN m
                   | isInfinite n = isInfinite m && signum n == signum m
                   | abs n == 0   = n == m
                   | otherwise    = isCloseEnough n m
  VBool b1 == VBool b2 = b1 == b2
  VUndefined == VUndefined = True
  _ == _ = False

type Store = [(String,Val)]

-- REPL commands
data Cmd = Eval Exp | Quit

command :: String -> Either String Cmd
command s
  | (take 5 s) == ":quit" = Right Quit
  | (head s) == ':' = Left $ "bad command"
  | otherwise = jsExp s >>= return . Eval

jsExp :: String -> Either String Exp
jsExp s =
  case parse s "input" of
    Left _ -> Left $ "JS parser failed"
    Right (JSAstProgram [] _) -> Right $ Lit VUndefined
    Right (JSAstProgram (s:ss) _) -> foldl (\t s -> do { u <- t; v <- parseJsStm s; return $ Seq u v }) (parseJsStm s) ss
    Right (JSAstStatement s _) -> parseJsStm s
    Right (JSAstExpression e _) -> parseJs e
    Right x -> Left $ "JS parser did not return an expression or statement"

parseJsStm :: JSStatement -> Either String Exp
parseJsStm (JSStatementBlock _ [] _ _) = Right $ Lit VUndefined
parseJsStm (JSStatementBlock _ (s:ss) _ _) = foldl (\t s -> do { u <- t; v <- parseJsStm s; return $ Seq u v }) (parseJsStm s) ss
parseJsStm (JSAssignStatement (JSIdentifier _ x) (JSAssign _) e _) = do { t <- parseJs e; return $ Assign x t }
parseJsStm (JSExpressionStatement e _) = parseJs e

parseJs :: JSExpression -> Either String Exp
parseJs (JSExpressionParen _ e _) = parseJs e
parseJs (JSDecimal _ s) = return $ Lit (VNum (read s :: Double))
parseJs (JSLiteral _ "true") = return $ Lit (VBool True)
parseJs (JSLiteral _ "false") = return $ Lit (VBool False)
parseJs (JSIdentifier _ "undefined") = return $ Lit VUndefined
parseJs (JSIdentifier _ x) = return $ Var x
parseJs (JSAssignExpression (JSIdentifier _ x)  (JSAssign _) e) = do { t <- parseJs e; return $ Assign x t }
parseJs (JSCommaExpression e1 _ e2) = Seq <$> parseJs e1 <*> parseJs e2
parseJs (JSExpressionBinary e1 (JSBinOpAnd _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin And t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpDivide _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Div t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpEq _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Eq t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpGe _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Ge t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpGt _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Gt t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpLe _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Le t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpLt _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Lt t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpMinus _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Sub t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpMod _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Mod t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpNeq _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Ne t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpOr _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Or t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpPlus _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Add t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpStrictEq _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin SEq t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpStrictNeq _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin SNe t1 t2 }
parseJs (JSExpressionBinary e1 (JSBinOpTimes _) e2) = do { t1 <- parseJs e1; t2 <- parseJs e2; return $ Bin Mul t1 t2 }
parseJs (JSExpressionTernary e0 _ e1 _ e2) = do { t0 <- parseJs e0; t1 <- parseJs e1; t2 <- parseJs e2; return $ Cond t0 t1 t2 }
parseJs (JSUnaryExpression (JSUnaryOpMinus _) e) = do { t <- parseJs e; return $ Unary Minus t }
parseJs (JSUnaryExpression (JSUnaryOpNot _) e) = do { t <- parseJs e; return $ Unary Not t }
parseJs (JSUnaryExpression (JSUnaryOpPlus _) e) = do { t <- parseJs e; return $ Unary Plus t }
parseJs e = Left $ "Cannot parse complex JS expression"

-- A simple pretty-printer for Exps
prettyVal :: Val -> String
prettyVal (VNum n) = show n
prettyVal (VBool True) = "true"
prettyVal (VBool False) = "false"
prettyVal (VUndefined) = "undefined"

pretty :: Exp -> String
pretty (Lit v) = prettyVal v
pretty e @ (Bin Add e1 e2) = (paren e1) ++ " + " ++ (paren e2)
pretty e @ (Bin Sub e1 e2) = (paren e1) ++ " - " ++ (paren e2)
pretty e @ (Bin Mul e1 e2) = (paren e1) ++ " * " ++ (paren e2)
pretty e @ (Bin Div e1 e2) = (paren e1) ++ " / " ++ (paren e2)
pretty e @ (Bin Mod e1 e2) = (paren e1) ++ " % " ++ (paren e2)
pretty e @ (Bin And e1 e2) = (paren e1) ++ " && " ++ (paren e2)
pretty e @ (Bin Or e1 e2) = (paren e1) ++ " || " ++ (paren e2)
pretty e @ (Bin Gt e1 e2) = (paren e1) ++ " > " ++ (paren e2)
pretty e @ (Bin Lt e1 e2) = (paren e1) ++ " < " ++ (paren e2)
pretty e @ (Bin Ge e1 e2) = (paren e1) ++ " >= " ++ (paren e2)
pretty e @ (Bin Le e1 e2) = (paren e1) ++ " <= " ++ (paren e2)
pretty e @ (Bin Eq e1 e2) = (paren e1) ++ " == " ++ (paren e2)
pretty e @ (Bin Ne e1 e2) = (paren e1) ++ " != " ++ (paren e2)
pretty e @ (Bin SEq e1 e2) = (paren e1) ++ " === " ++ (paren e2)
pretty e @ (Bin SNe e1 e2) = (paren e1) ++ " !== " ++ (paren e2)
pretty (Cond c t e) = (paren c) ++ " ? " ++ (paren t) ++ " : " ++ (paren e)
pretty (Unary Plus e@(Unary Plus _)) = "+(" ++ pretty e ++ ")"
pretty (Unary Minus e@(Unary Minus _)) = "-(" ++ pretty e ++ ")"
pretty (Unary Minus (Lit (VNum n))) | n < 0 = "-(" ++ show n ++ ")"
pretty (Unary Plus e) = "+" ++ (paren e)
pretty (Unary Minus e) = "-" ++ paren e
pretty (Unary Not e) = "!" ++ (paren e)
pretty (Var x) = x
pretty (Assign x e) = x ++ " = " ++ paren e
pretty (Seq e1 e2) = paren e1 ++ ", " ++ paren e2

paren :: Exp -> String
paren e @ (Bin _ _ _) = "(" ++ pretty e ++ ")"
paren e@Assign{} = "(" ++ pretty e ++ ")"
paren e@Seq{} = "(" ++ pretty e ++ ")"
paren e@Cond{} = "(" ++ pretty e ++ ")"
paren e @ _           = pretty e
