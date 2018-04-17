module MInt (intMethods) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Text.Read (readMaybe)

import AbsGrammar
import MInterpreter
import MUtil

intMethods :: Methods
intMethods = DataMap.fromList [
  (Ident "ToString", intToString)]

ensureInt :: Var -> Interpreter Integer
ensureInt value = case value of
  VInt int -> return int
  _ -> throwError "Integer expected."

intToString :: Ident -> [Var] -> Interpreter Var
intToString self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (_, value) <- startMethod self
  int <- ensureInt value
  return $ VString $ show int
