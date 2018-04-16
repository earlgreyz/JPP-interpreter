module MUtil where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import MInterpreter

-- Get a value at key @k from map @m or raise an error @err
mustGet :: Show k => Ord k => DataMap.Map k a -> k -> String -> Interpreter a
mustGet m k err = case m DataMap.!? k of
  Nothing -> throwError $ show k ++ err
  Just v -> return v

-- Finds the self object for the method and returns it
startMethod :: Ident -> Interpreter (Loc, Var)
startMethod self = do
  env <- ask
  store <- get
  location <- mustGet env self " was not declared in this scope."
  value <- mustGet store location " has not yet been initialized."
  return (location, value)

-- Finds an unused location in the intrepreter environment and returns it
allocate :: Type -> Interpreter Loc
allocate t = do
  env <- ask
  store <- get
  let maxEnd = if (DataMap.null env) then 0 else maximum $ map fst (DataMap.elems env)
  let maxStore = if (DataMap.null store) then 0 else maximum $ map fst (DataMap.keys store)
  let location = (maximum [maxStore, maxEnd]) + 1
  return (location, t)
