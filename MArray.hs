module MArray (arrayMethods) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import IInterpreter
import IUtil

arrayMethods :: Methods
arrayMethods = DataMap.fromList [
  (Ident "Append", arrayAppend),
  (Ident "At", arrayAt),
  (Ident "Put", arrayPut),
  (Ident "Length", arrayLength)]

ensureArray :: Var -> Interpreter [Var]
ensureArray value = case value of
  VArray array -> return array
  _ -> throwError "Array expected."

ensureIndexInBounds :: [Var] -> Int -> Interpreter ()
ensureIndexInBounds array index = do
  unless (index >= 0 && index < length array) $ throwError "Index out of bound."

ensureInt :: Var -> Interpreter Int
ensureInt value = case value of
  VInt integer -> return $ fromIntegral integer
  _ -> throwError "Integer expected."

arrayAppend :: Ident -> [Var] -> Interpreter Var
arrayAppend self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (location, value) <- startMethod self
  array <- ensureArray value
  modify $ DataMap.insert location $ VArray (array ++ args)
  return VNone

arrayAt :: Ident -> [Var] -> Interpreter Var
arrayAt self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (_, value) <- startMethod self
  array <- ensureArray value
  index <- ensureInt $ args !! 0
  ensureIndexInBounds array index
  return $ array !! index

arrayPut :: Ident -> [Var] -> Interpreter Var
arrayPut self args = do
  unless (length args == 2) $ throwError "Expected two arguments."
  (location, value) <- startMethod self
  array <- ensureArray value
  index <- ensureInt $ args !! 0
  ensureIndexInBounds array index
  let arg = args !! 1
  let newArray = (take index array) ++ [arg] ++ (drop (index + 1) array)
  modify $ DataMap.insert location $ VArray newArray
  return VNone

arrayLength :: Ident -> [Var] -> Interpreter Var
arrayLength self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (location, value) <- startMethod self
  array <- ensureArray value
  return $ VInt $ toInteger (length array)
