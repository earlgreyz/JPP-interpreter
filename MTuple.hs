module MTuple (tupleMethods) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import IInterpreter
import IUtil

tupleMethods :: Methods
tupleMethods = DataMap.fromList [
  (Ident "Extract", tupleExtract),
  (Ident "Exchange", tupleExchange)]

ensureTuple :: Var -> Interpreter [Var]
ensureTuple value = case value of
  VTuple tuple -> return tuple
  _ -> throwError "Tuple expected."

ensureIndexInBounds :: [Var] -> Int -> Interpreter ()
ensureIndexInBounds tuple index = do
  unless (index >= 0 && index < length tuple) $ throwError "Index out of bound."

ensureInt :: Var -> Interpreter Int
ensureInt value = case value of
  VInt integer -> return $ fromIntegral integer
  _ -> throwError "Integer expected."

tupleExtract :: Ident -> [Var] -> Interpreter Var
tupleExtract self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (_, value) <- startMethod self
  tuple <- ensureTuple value
  index <- ensureInt $ args !! 0
  ensureIndexInBounds tuple index
  return $ tuple !! index

tupleExchange :: Ident -> [Var] -> Interpreter Var
tupleExchange self args = do
  unless (length args == 2) $ throwError "Expected two arguments."
  (location, value) <- startMethod self
  tuple <- ensureTuple value
  index <- ensureInt $ args !! 0
  ensureIndexInBounds tuple index
  let arg = args !! 1
  let newTuple = (take index tuple) ++ [arg] ++ (drop (index + 1) tuple)
  modify $ DataMap.insert location $ VTuple newTuple
  return VNone
