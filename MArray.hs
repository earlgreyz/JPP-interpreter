module MArray (arrayMethods, arrayMethodTypes) where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import IInterpreter
import IUtil
import TCheck
import TUtil

arrayMethods :: Methods
arrayMethods = DataMap.fromList [
  (Ident "Append", arrayAppend),
  (Ident "At", arrayAt),
  (Ident "Put", arrayPut),
  (Ident "Length", arrayLength)]

arrayMethodTypes :: MethodTypes
arrayMethodTypes = DataMap.fromList [
  (Ident "Append", arrayAppendType),
  (Ident "At", arrayAtType),
  (Ident "Put", arrayPutType),
  (Ident "Length", arrayLengthType)]

ensureArray :: Var -> Interpreter [Var]
ensureArray value = case value of
  VArray array -> return array
  _ -> throwError "Array expected."

ensureArrayType :: Type -> TypeCheck Type
ensureArrayType value = case value of
  TArray t -> return t
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

arrayAppendType :: Ident -> [Type] -> TypeCheck Type
arrayAppendType self _ = do
  st <- startMethodType self
  t <- ensureArrayType st
  return $ TFunc [t] TNone

arrayAt :: Ident -> [Var] -> Interpreter Var
arrayAt self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (_, value) <- startMethod self
  array <- ensureArray value
  index <- ensureInt $ args !! 0
  ensureIndexInBounds array index
  return $ array !! index

arrayAtType :: Ident -> [Type] -> TypeCheck Type
arrayAtType self _ = do
  st <- startMethodType self
  t <- ensureArrayType st
  return $ TFunc [TInt] t

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

arrayPutType :: Ident -> [Type] -> TypeCheck Type
arrayPutType self _ = do
  st <- startMethodType self
  t <- ensureArrayType st
  return $ TFunc [TInt, t] TNone

arrayLength :: Ident -> [Var] -> Interpreter Var
arrayLength self args = do
  unless (length args == 0) $ throwError "Expected no arguments."
  (location, value) <- startMethod self
  array <- ensureArray value
  return $ VInt $ toInteger (length array)

arrayLengthType :: Ident -> [Type] -> TypeCheck Type
arrayLengthType _ _ = return $ TFunc [] TInt
