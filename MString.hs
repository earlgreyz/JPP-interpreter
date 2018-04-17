module MString (stringMethods) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Text.Read (readMaybe)

import AbsGrammar
import MInterpreter
import MUtil

stringMethods :: Methods
stringMethods = DataMap.fromList [
  (Ident "ToInt", stringToInt)]

ensureString :: Var -> Interpreter String
ensureString value = case value of
  VString string -> return string
  _ -> throwError "String expected."

stringToInt :: Ident -> [Var] -> Interpreter Var
stringToInt self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (_, value) <- startMethod self
  string <- ensureString value
  case readMaybe string of
    Just int -> return $ VInt int
    Nothing -> throwError $ "Unable to convert \"" ++ string ++ "\""
