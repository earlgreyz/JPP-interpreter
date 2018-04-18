module MError (errorMethods) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import IInterpreter
import IUtil

errorMethods :: Methods
errorMethods = DataMap.fromList [
  (Ident "HaveOccurred", errorHaveOccurred)]

ensureError :: Var -> Interpreter String
ensureError value = case value of
  VError err -> return err
  _ -> throwError "Error expected."

errorHaveOccurred :: Ident -> [Var] -> Interpreter Var
errorHaveOccurred self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (_, value) <- startMethod self
  err <- ensureError value
  return $ VBool $ err /= "``"
