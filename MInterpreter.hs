module MInterpreter where

import System.IO ( stderr, hPutStrLn, putStrLn )
import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Reader
import AbsGrammar

-- Special location used for return value
returnLocation :: Loc
returnLocation = (-1, TInt)

-- type Function = [Variable] -> Interpreter Variable

data Variable =
  VInt Integer
  | VBool Bool
  | VString String
  | VError String
  | VArray [Variable]
  | VNone

instance Eq Variable where
  (VInt x) == (VInt y) = x == y
  (VBool x) == (VBool y) = x == y
  (VString x) == (VString y) = x == y
  (VError x) == (VError y) = x == y
  _ == _ = False

instance Show Variable where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VString s) = "`" ++ show s ++ "`"
  show (VError e) = "\"" ++ show e ++ "\""
  show (VArray xs) = "[" ++ stringify xs ++ "]" where
    stringify :: [Variable] -> String
    stringify [] = []
    stringify [x] = show x
    stringify (x:xs) = (show x) ++ ", " ++ (stringify xs)
  show VNone = "None"

type Loc = (Int, Type)
type Env = DataMap.Map Ident Loc
type Store = DataMap.Map Loc Variable
-- type FStore = Map Ident (Function, Type)

type MError = ErrorT String IO
type StetefulError = StateT Store MError
type Interpreter = ReaderT Env StetefulError

evalBool :: Exp -> Interpreter Bool
evalBool e = do
  val <- evalExp e
  case val of
    VBool b -> return b
    _ -> throwError $ "Boolean expected."

evalInt :: Exp -> Interpreter Integer
evalInt e = do
  val <- evalExp e
  case val of
    VInt n -> return n
    _ -> throwError $ "Integer expected."

evalExp :: Exp -> Interpreter Variable
evalExp (ECall _) = throwError $ "Not implemented yet." -- TODO: implement
evalExp (EVar x) = do
  env <- ask
  store <- get
  location <- case env DataMap.!? x of
    Nothing -> throwError $ show x ++ " was not declared in this scope."
    Just loc -> return loc
  value <- case store DataMap.!? location of
    Nothing -> throwError $ show x ++ " has not yet been initialized."
    Just val -> return val
  return value
evalExp (ELit l) = case l of
  LInt n -> return $ VInt n
  LBool b -> let b' = case b of { BTrue -> True; BFalse -> False}
    in return $ VBool b'
  LStr s -> return $ VString s
  LErr (TokenError e) -> return $ VError e
  _ -> throwError "Not implemented yet." -- TODO: implement
evalExp (ETimes e f) = fmap VInt $ liftM2 (*) (evalInt e) (evalInt f)
evalExp (EDiv e f) = fmap VInt $ liftM2 (div) (evalInt e) (evalInt f)
evalExp (EMod e f) = fmap VInt $ liftM2 (mod) (evalInt e) (evalInt f)
evalExp (EPlus e f) = fmap VInt $ liftM2 (+) (evalInt e) (evalInt f)
evalExp (EMinus e f) = fmap VInt $ liftM2 (-) (evalInt e) (evalInt f)
evalExp (EComp e c f) = do
  cmp <- case c of
    CLt -> return (<)
    CGt -> return (>)
    CLe -> return (<=)
    CGe -> return (>=)
    CEq -> return (==)
  fmap VBool $ liftM2 cmp (evalInt e) (evalInt f)
evalExp (EBool e b f) = do
  op <- case b of
    BAnd -> return (&&)
    BOr -> return (||)
  fmap VBool $ liftM2 op (evalBool e) (evalBool f)

execDecl :: Decl -> Interpreter () -> Interpreter ()
execDecl (DVar x t v) interpreter = do
  env <- ask
  store <- get
  let maxEnd = if (DataMap.null env) then 0 else maximum $ map fst (DataMap.elems env)
  let maxStore = if (DataMap.null store) then 0 else maximum $ map fst (DataMap.keys store)
  let locNumber = (maximum [maxStore, maxEnd]) + 1
  let location = (locNumber, t)
  value <- evalExp v
  modify $ DataMap.insert location value
  local (DataMap.insert x location) interpreter

execStmt :: Stmt -> Interpreter ()
execStmt (SBlock d s) = foldr execDecl (execManyStmt s) d
execStmt (SAssign x e) = do
  env <- ask
  location <- case env DataMap.!? x of
    Nothing -> throwError $ show x ++ " was not declared in this scope."
    Just loc -> return loc
  value <- evalExp e
  modify $ DataMap.insert location value
execStmt (SUnpack xs e) = throwError $ "Not implemented yet." -- TODO: implement
execStmt (SReturn r) = case r of
  RExp e -> do
    value <- evalExp e
    modify $ DataMap.insert returnLocation value
  RNone -> do
    modify $ DataMap.insert returnLocation VNone
execStmt (SPrint e) = do
  value <- evalExp e
  liftIO . putStrLn . show $ value
execStmt (SIf b s es) = throwError $ "Not implemented yet." -- TODO: implement
execStmt (SIfelse b s es e) = throwError $ "Not implemented yet." -- TODO: implement
execStmt (SWhile b s) = throwError $ "Not implemented yet." -- TODO: implement
execStmt SBreak = throwError $ "Not implemented yet." -- TODO: implement
execStmt SCont = throwError $ "Not implemented yet." -- TODO: implement
execStmt (SCall _) = throwError $ "Not implemented yet." -- TODO: implement

execManyStmt :: [Stmt] -> Interpreter ()
execManyStmt l = foldM (\_ -> execStmt) () l

exec :: Program -> IO ()
exec (Prog d s) = do
  let initState = DataMap.fromList []
  let initStore = DataMap.empty
  result <- runErrorT $ flip runStateT initState $ flip runReaderT initStore $ execStmt (SBlock d s)
  case result of
    Left err -> hPutStrLn stderr $ "Runtime Error: " ++ err
    Right _ -> return () -- putStrLn $ showStore store
