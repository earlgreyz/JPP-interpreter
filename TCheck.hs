module TCheck where

import System.IO
import qualified Data.Map as DataMap
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar

type Env = DataMap.Map Ident Type

type MethodTypes = DataMap.Map Ident (Ident -> [Type] -> TypeCheck Type)

type TExcept = ExceptT String IO
type TypeCheck = ReaderT Env TExcept
