module PrettyTest where

import           Control.Monad
import           Data.Generics.Uniplate.Data
import           Data.Generics.Uniplate.Operations
import           Hedgehog

import           Generator
import           JS

removeNegativeLits :: Exp -> Exp
removeNegativeLits = transform f where
  f (Lit (VNum n)) | n < 0 = Unary Minus (Lit (VNum (- n)))
  f x = x

main :: IO ()
main = void $ check $ property $ do
  e <- removeNegativeLits <$> forAll genSeqArith
  -- e <- forAll genExp
  tripping e pretty jsExp
