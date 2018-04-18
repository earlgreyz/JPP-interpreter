module MError (errorMethods, errorMethodTypes) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import IInterpreter
import IUtil
import TCheck
import TUtil

errorMethods :: Methods
errorMethods = DataMap.fromList [
  (Ident "HaveOccurred", errorHaveOccurred)]

errorMethodTypes :: MethodTypes
errorMethodTypes = DataMap.fromList [
  (Ident "HaveOccurred", errorHaveOccurredType)]

ensureError :: Var -> Interpreter String
ensureError value = case value of
  VError err -> return err
  _ -> throwError "Error expected."

ensureErrorType :: Type -> TypeCheck Type
ensureErrorType value = case value of
  TError -> return TError
  _ -> throwError "Error expected."

errorHaveOccurred :: Ident -> [Var] -> Interpreter Var
errorHaveOccurred self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (_, value) <- startMethod self
  err <- ensureError value
  return $ VBool $ err /= "``"

errorHaveOccurredType :: Ident -> [Type] -> TypeCheck Type
errorHaveOccurredType self _ = do
  st <- startMethodType self
  ensureErrorType st
  return $ TFunc [] TBool
