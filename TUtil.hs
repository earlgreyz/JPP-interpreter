module TUtil where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import Util
import TCheck

isDeclarableType :: Type -> Bool
isDeclarableType TAny = False
isDeclarableType TNone = False
isDeclarableType TInt = True
isDeclarableType TBool = True
isDeclarableType TString = True
isDeclarableType TError = True
isDeclarableType (TArray ts) = isDeclarableType ts
isDeclarableType (TTuple ts) = (ts /= []) && (all isDeclarableType ts)
isDeclarableType (TFunc ts r) = (all isDeclarableType ts) && r /= TAny

canAssign :: Type -> Type -> Bool
canAssign (TArray _) (TArray TAny) = True
canAssign (TArray a) (TArray b) = canAssign a b
canAssign (TTuple a) (TTuple b) = all (\(x, y) -> canAssign x y) $ zip a b
canAssign a b = a == b

startMethodType :: Ident -> TypeCheck Type
startMethodType self = do
  env <- ask
  t <- mustGet env self " was not declared in this scope."
  return t
