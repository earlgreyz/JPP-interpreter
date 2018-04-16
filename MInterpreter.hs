module MInterpreter where

import System.IO
import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar

data Var =
  VInt Integer
  | VBool Bool
  | VString String
  | VError String
  | VArray [Var]
  | VTuple [Var]
  | VFunc ([Exp] -> Interpreter Var)
  | VNone

type Loc = (Int, Type)
type Store = DataMap.Map Loc Var
type Env = DataMap.Map Ident Loc
type Methods = DataMap.Map Ident (Ident -> [Var] -> Interpreter Var)

type MExcept = ExceptT String IO
type MState = StateT Store MExcept
type Interpreter = ReaderT Env MState

instance Eq Var where
  (VInt x) == (VInt y) = x == y
  (VBool x) == (VBool y) = x == y
  (VString x) == (VString y) = x == y
  (VError x) == (VError y) = x == y
  (VArray x) == (VArray y) = x == y
  (VTuple x) == (VTuple y) = x == y
  _ == _ = False

instance Show Var where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VString s) = s
  show (VError e) = e
  show (VArray xs) = "[" ++ stringifyList xs ++ "]"
  show (VTuple xs) = "<|" ++ stringifyList xs ++ "|>"
  show VNone = "None"

stringifyList :: Show a => [a] -> String
stringifyList [] = []
stringifyList [x] = show x
stringifyList (x:xs) = (show x) ++ ", " ++ (stringifyList xs)
