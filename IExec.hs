module IExec (exec) where

import System.IO ( stderr, hPutStrLn, putStrLn, putStr )
import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import Util
import IInterpreter
import IUtil

import MArray
import MTuple
import MError
import MString
import MInt

import TExec

-- Methods map
allMethods :: Methods
allMethods = DataMap.unions [arrayMethods, tupleMethods, errorMethods, stringMethods, intMethods]

-- Special location used for return value
returnLocation :: Loc
returnLocation = -1

-- Special location used for loop state
loopLocation :: Loc
loopLocation = -2

loopBreak    = VInt (-1)
loopContinue = VInt (1)
loopNormal   = VInt (0)

-- Evaluates an expression and returns a bool or raises and exception
evalBool :: Exp -> Interpreter Bool
evalBool e = do
  val <- evalExp e
  case val of
    VBool b -> return b
    _ -> throwError $ "Boolean expected."

-- Evaluates an expression and returns an int or raises and exception
evalInt :: Exp -> Interpreter Integer
evalInt e = do
  val <- evalExp e
  case val of
    VInt n -> return n
    _ -> throwError $ "Integer expected."

-- Evaluates an expression and returns its value
evalExp :: Exp -> Interpreter Var
evalExp (ECall c) = case c of
  (CFun f es) -> do
    env <- ask
    store <- get
    location <- mustGet env f " was not declared in this scope."
    value <- mustGet store location " has not yet been initialized."
    case value of
      VFunc func -> func es
      _ -> throwError $ show f ++ " is not a function."
  (CMet s m es) -> do
    met <- mustGet allMethods m " was not declared in this scope."
    args <- mapM evalExp es
    met s args
evalExp (EVar x) = do
  env <- ask
  store <- get
  location <- mustGet env x " was not declared in this scope."
  mustGet store location " has not yet been initialized."
evalExp (ELit l) = case l of
  LNon -> return VNone
  LInt n -> return $ VInt n
  LBool b -> return $ VBool $ if b == BTrue then True else False
  LStr s -> return $ VString s
  LErr (TokenError e) -> return $ VError e
  LArr es -> mapM evalExp es >>= return . VArray
  LTup es -> mapM evalExp es >>= return . VTuple
evalExp (ETimes e f) = fmap VInt $ liftM2 (*) (evalInt e) (evalInt f)
evalExp (EDiv e f) = do
  n <- evalInt e
  m <- evalInt f
  when (m == 0) $ throwError "Dividing by 0 is not allowed."
  return $ VInt $ div n m
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

-- Executes a declaration and returns and Interpreter with its environment changed.
execDecl :: Decl -> Interpreter () -> Interpreter ()
execDecl (DVar x t v) interpreter = do
  location <- allocate
  value <- evalExp v
  modify $ DataMap.insert location value
  local (DataMap.insert x location) interpreter
execDecl (DFunc f ps r s) interpreter = do
  location <- allocate
  env <- ask
  modify $ DataMap.insert location $ VFunc (function f r env location ps s )
  local (DataMap.insert f location) interpreter
  where
    execArg :: Env -> (Param, Exp) -> Interpreter Env
    execArg env ((PVal x _), e) = do
      location <- allocate
      value <- evalExp e
      modify $ DataMap.insert location value
      return $ DataMap.insert x location env
    function :: Ident -> Type -> Env -> Loc -> [Param] -> Stmt -> [Exp] -> Interpreter Var
    function f t env location ps s vs = do
        env' <- foldM execArg env $ zip ps vs
        local (\_ -> DataMap.insert f location env') $ do
          execStmt s
          store <- get
          ret <- case store DataMap.!? returnLocation of
            Nothing -> if t == TNone then return VNone else
              throwError $ "Missing return statement."
            Just value -> return value
          modify $ DataMap.delete returnLocation
          return ret

-- Executes a single statement
execStmt :: Stmt -> Interpreter ()
execStmt (SBlock d s) = foldr execDecl (execManyStmt s) d
execStmt (SAssign x e) = do
  env <- ask
  location <- mustGet env x " was not declared in this scope."
  value <- evalExp e
  modify $ DataMap.insert location value
execStmt (SUnpack xs e) = do
  tuple <- evalExp e
  vs <- case tuple of
    VTuple tup -> return tup
    _ -> throwError $ "Tuple expected."
  mapM_ assign $ zip xs vs
  where
    assign (x, v) = do
      env <- ask
      location <- mustGet env x " was not declared in this scope."
      modify $ DataMap.insert location v
execStmt (SReturn e) = do
  value <- evalExp e
  modify $ DataMap.insert returnLocation value
execStmt (SPrint es) = do
  mapM_ (\e -> evalExp e >>= liftIO . putStr . show) es
  liftIO . putStr $ "\n"
execStmt (SIf b s es) = do
  cond <- evalBool b
  if cond then execStmt s else case es of
    [] -> return ()
    (EElif b' s'):t -> execStmt (SIf b' s' t)
execStmt (SIfelse b s es e) = do
  cond <- evalBool b
  if cond then execStmt s else case es of
    [] -> execStmt e
    (EElif b' s'):t -> execStmt (SIfelse b' s' t e)
execStmt while@(SWhile b s) = do
  cond <- evalBool b
  when cond $ do
    execStmt s
    store <- get
    loopState <- case store DataMap.!? loopLocation of
      Nothing -> throwError $ "Break/Continue called outside of the loop."
      Just loop -> return loop
    modify $ DataMap.insert loopLocation loopNormal
    when (loopState /= loopBreak) (execStmtOrSkip while)
execStmt SBreak = modify $ DataMap.insert loopLocation loopBreak
execStmt SCont = modify $ DataMap.insert loopLocation loopContinue
execStmt (SCall c) = evalExp (ECall c) >> return ()

-- Executes a single statement unless a special case ocurred
-- * a return was called
-- * a break was called
-- * a continue was called
execStmtOrSkip :: Stmt -> Interpreter ()
execStmtOrSkip s = do
  store <- get
  case store DataMap.!? returnLocation of
    Just _ -> return ()
    Nothing -> case store DataMap.!? loopLocation of
      Nothing -> throwError $ "Break/Continue called outside of the loop."
      Just loop -> when (loop == loopNormal) (execStmt s)

-- Executes many statements
execManyStmt :: [Stmt] -> Interpreter ()
execManyStmt l = mapM_ execStmtOrSkip l

-- Executes the program
exec :: Program -> IO ()
exec program@(Prog d s) = do
  typeResult <- runExceptT $ execType program
  case typeResult of
    Left err -> hPutStrLn stderr $ "Type Error: " ++ err
    Right _ -> do
      let initStore = DataMap.fromList [(loopLocation, loopNormal)]
      let initEnv = DataMap.empty
      result <- runExceptT $ flip runStateT initStore $ flip runReaderT initEnv $ execStmt (SBlock d s)
      case result of
        Left err -> hPutStrLn stderr $ "Runtime Error: " ++ err
        Right _ -> return ()
