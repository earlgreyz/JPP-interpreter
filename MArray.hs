module MArray where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import MInterpreter
import MUtil

arrayMethods :: Methods
arrayMethods = DataMap.fromList [(Ident "Append", arrayAppend), (Ident "At", arrayAt)]

arrayAppend :: Ident -> [Var] -> Interpreter Var
arrayAppend self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (location, value) <- startMethod self
  case value of
    VArray arr -> do
      modify $ DataMap.insert location $ VArray (arr ++ args)
      return VNone
    _ -> throwError "Array expected."

arrayAt :: Ident -> [Var] -> Interpreter Var
arrayAt self args = do
  unless (length args == 1) $ throwError "Expected one argument."
  (_, value) <- startMethod self
  case value of
    VArray arr -> case args !! 0 of
      VInt index -> return $ arr !! (fromIntegral index)
    _ -> throwError "Array expected."
