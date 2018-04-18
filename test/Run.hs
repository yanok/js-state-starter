module Run where

import           System.Process
import           Text.Read

import           JS

parseVal :: String -> Maybe Val
parseVal "undefined" = Just VUndefined
parseVal "false" = Just $ VBool False
parseVal "true" = Just $ VBool True
parseVal n = VNum <$> readMaybe n

runWith :: FilePath -> [String] -> String -> IO String
runWith p args input = readProcess p args input

runInNode :: Exp -> IO (Either String Val)
runInNode e = do
  s <- runWith "node" ["-p"] $ show e
  case lines s of
    [l] -> case parseVal l of
      Nothing -> return $ Left s
      Just v -> return $ Right v
    _ -> return $ Left s
