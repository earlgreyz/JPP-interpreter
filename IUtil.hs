module IUtil where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import Util
import IInterpreter

-- Finds the self object for the method and returns it
startMethod :: Ident -> Interpreter (Loc, Var)
startMethod self = do
  env <- ask
  store <- get
  location <- mustGet env self " was not declared in this scope."
  value <- mustGet store location " has not yet been initialized."
  return (location, value)

-- Finds an unused location in the intrepreter environment and returns it
allocate :: Interpreter Loc
allocate = do
  env <- ask
  store <- get
  let maxEnd = if (DataMap.null env) then 0 else maximum $ DataMap.elems env
  let maxStore = if (DataMap.null store) then 0 else maximum $ DataMap.keys store
  return $ (maximum [maxStore, maxEnd]) + 1
