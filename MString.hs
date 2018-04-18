module MString (stringMethods, stringMethodTypes) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Text.Read (readMaybe)

import AbsGrammar
import IInterpreter
import IUtil
import TCheck
import TUtil

stringMethods :: Methods
stringMethods = DataMap.fromList [
  (Ident "ToInt", stringToInt)]

stringMethodTypes :: MethodTypes
stringMethodTypes = DataMap.fromList [
  (Ident "ToInt", stringToIntType)]

ensureString :: Var -> Interpreter String
ensureString value = case value of
  VString string -> return string
  _ -> throwError "String expected."

ensureStringType :: Type -> TypeCheck Type
ensureStringType value = case value of
  TString -> return TString
  _ -> throwError "String expected."

stringToInt :: Ident -> [Var] -> Interpreter Var
stringToInt self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (_, value) <- startMethod self
  string <- ensureString value
  case readMaybe string of
    Just int -> return $ VInt int
    Nothing -> throwError $ "Unable to convert \"" ++ string ++ "\""

stringToIntType :: Ident -> [Type] -> TypeCheck Type
stringToIntType self _ = do
  st <- startMethodType self
  ensureStringType st
  return $ TFunc [] TInt
