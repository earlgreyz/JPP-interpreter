module MInt (intMethods, intMethodTypes) where

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

intMethods :: Methods
intMethods = DataMap.fromList [
  (Ident "ToString", intToString)]

intMethodTypes :: MethodTypes
intMethodTypes = DataMap.fromList [
  (Ident "ToString", intToStringType)]

ensureInt :: Var -> Interpreter Integer
ensureInt value = case value of
  VInt int -> return int
  _ -> throwError "Integer expected."

ensureIntType :: Type -> TypeCheck Type
ensureIntType value = case value of
  TInt -> return TInt
  _ -> throwError "Integer expected."

intToString :: Ident -> [Var] -> Interpreter Var
intToString self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (_, value) <- startMethod self
  int <- ensureInt value
  return $ VString $ show int

intToStringType :: Ident -> [Type] -> TypeCheck Type
intToStringType self _ = do
  st <- startMethodType self
  ensureIntType st
  return $ TFunc [] TString
