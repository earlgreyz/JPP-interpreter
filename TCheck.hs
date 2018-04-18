module TCheck where

import System.IO
import qualified Data.Map as DataMap
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar

type Env = DataMap.Map Ident Type

type MethodType = Ident -> [Exp] -> TypeCheck Type
type MethodTypes = DataMap.Map Ident MethodType

type TExcept = ExceptT String IO
type TypeCheck = ReaderT Env TExcept
