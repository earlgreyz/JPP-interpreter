module MTuple (tupleMethods, tupleMethodTypes) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import IInterpreter
import IUtil
import TCheck
import TUtil

tupleMethods :: Methods
tupleMethods = DataMap.fromList [
  (Ident "Extract", tupleExtract),
  (Ident "Exchange", tupleExchange)]

tupleMethodTypes :: MethodTypes
tupleMethodTypes = DataMap.fromList [
  (Ident "Extract", tupleExtractType),
  (Ident "Exchange", tupleExchangeType)]

ensureTuple :: Var -> Interpreter [Var]
ensureTuple value = case value of
  VTuple tuple -> return tuple
  _ -> throwError "Tuple expected."

ensureTupleType :: Type -> TypeCheck [Type]
ensureTupleType value = case value of
  TTuple tuple -> return tuple
  _ -> throwError "Tuple expected."

ensureIndexInBounds :: [Var] -> Int -> Interpreter ()
ensureIndexInBounds tuple index = do
  unless (index >= 0 && index < length tuple) $ throwError "Index out of bound."

ensureInt :: Var -> Interpreter Int
ensureInt value = case value of
  VInt integer -> return $ fromIntegral integer
  _ -> throwError "Integer expected."

ensureIntType :: Exp -> TypeCheck Int
ensureIntType value = case value of
  ELit l -> case l of
    LInt i -> return $ fromIntegral i
    _ -> throwError "Dynamic index is not supported for tuple methods."
  _ -> throwError "Dynamic index is not supported for tuple methods."

tupleExtract :: Method
tupleExtract self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (_, value) <- startMethod self
  tuple <- ensureTuple value
  index <- ensureInt $ args !! 0
  ensureIndexInBounds tuple index
  return $ tuple !! index

tupleExtractType :: MethodType
tupleExtractType self args = do
  t <- startMethodType self
  unless (length args == 1) $ throwError "Expected one argument."
  index <- ensureIntType $ args !! 0
  ts <- ensureTupleType t
  unless (index < length ts) $ throwError "Index out of bound."
  return $ TFunc [TInt] (ts !! index)

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

tupleExchangeType :: MethodType
tupleExchangeType self args = do
  t <- startMethodType self
  unless (length args == 2) $ throwError "Expected two arguments."
  index <- ensureIntType $ args !! 0
  ts <- ensureTupleType t
  unless (index < length ts) $ throwError "Index out of bound."
  return $ TFunc [TInt, (ts !! index)] TNone
