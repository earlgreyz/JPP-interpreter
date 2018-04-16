module MInterpreter where

import System.IO ( stderr, hPutStrLn, putStrLn )
import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import AbsGrammar

-- Special location used for return value
returnLocation :: Loc
returnLocation = (-1, TInt)

-- Special location used for loop state
loopLocation :: Loc
loopLocation = (-2, TInt)

loopBreak    = VInt (-1)
loopContinue = VInt (1)
loopNormal   = VInt (0)

data Variable =
  VInt Integer
  | VBool Bool
  | VString String
  | VError String
  | VArray [Variable]
  | VTuple [Variable]
  | VFunc ([Exp] -> Interpreter Variable)
  | VNone

instance Eq Variable where
  (VInt x) == (VInt y) = x == y
  (VBool x) == (VBool y) = x == y
  (VString x) == (VString y) = x == y
  (VError x) == (VError y) = x == y
  _ == _ = False

stringify :: [Variable] -> String
stringify [] = []
stringify [x] = show x
stringify (x:xs) = (show x) ++ ", " ++ (stringify xs)

instance Show Variable where
  show (VInt n) = show n
  show (VBool b) = show b
  show (VString s) = "\"" ++ s ++ "\""
  show (VError e) = "`" ++ e ++ "`"
  show (VArray xs) = "[" ++ stringify xs ++ "]"
  show (VTuple xs) = "<|" ++ stringify xs ++ "|>"
  show VNone = "None"

type Loc = (Int, Type)
type Store = DataMap.Map Loc Variable
type Env = DataMap.Map Ident Loc

type MExcept = ExceptT String IO
type MState = StateT Store MExcept
type Interpreter = ReaderT Env MState

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
evalExp (ECall c) = do
  case c of
    (CFun f es) -> do
      env <- ask
      store <- get
      location <- case env DataMap.!? f of
        Nothing -> throwError $ show f ++ " was not declared in this scope."
        Just loc -> return loc
      value <- case store DataMap.!? location of
        Nothing -> throwError $ show f ++ " has not yet been initialized."
        Just val -> return val
      case value of
        VFunc func -> do
          ret <- func es
          return ret
        _ -> throwError $ show f ++ " is not a function."
    (CMet _ _ _) -> throwError $ "Not implemented yet." -- TODO: implement
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

allocate :: Type -> Interpreter Loc
allocate t = do
  env <- ask
  store <- get
  let maxEnd = if (DataMap.null env) then 0 else maximum $ map fst (DataMap.elems env)
  let maxStore = if (DataMap.null store) then 0 else maximum $ map fst (DataMap.keys store)
  let location = (maximum [maxStore, maxEnd]) + 1
  return (location, t)

execDecl :: Decl -> Interpreter () -> Interpreter ()
execDecl (DVar x t v) interpreter = do
  location <- allocate t
  value <- evalExp v
  modify $ DataMap.insert location value
  local (DataMap.insert x location) interpreter
execDecl (DFunc f ps r s) interpreter = do
  location <- allocate TInt
  env <- ask
  modify $ DataMap.insert location $ VFunc (function f env location ps s )
  local (DataMap.insert f location) interpreter
  where
    makeDecl :: (Param, Exp) -> Decl
    makeDecl ((PVal x t), e) = (DVar x t e)
    execDeclWithEnv :: Env -> Decl -> Interpreter Env
    execDeclWithEnv env (DVar x t e) = do
      location <- allocate t
      value <- evalExp e
      modify $ DataMap.insert location value
      return $ DataMap.insert x location env
    function :: Ident -> Env -> Loc -> [Param] -> Stmt -> [Exp] -> Interpreter Variable
    function f env location ps s vs = do
        env' <- foldM execDeclWithEnv env $ map makeDecl (zip ps vs)
        local (\_ -> DataMap.insert f location env') $ do
          execStmt s
          store <- get
          ret <- case store DataMap.!? returnLocation of
            Nothing -> throwError $ "Missing return statement."
            Just value -> return value
          modify $ DataMap.delete returnLocation
          return ret

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

execStmtOrSkip :: Stmt -> Interpreter ()
execStmtOrSkip s = do
  store <- get
  case store DataMap.!? returnLocation of
    Just _ -> return ()
    Nothing -> case store DataMap.!? loopLocation of
      Nothing -> throwError $ "Break/Continue called outside of the loop."
      Just loop -> when (loop == loopNormal) (execStmt s)

execManyStmt :: [Stmt] -> Interpreter ()
execManyStmt l = foldM (\_ -> execStmtOrSkip) () l

exec :: Program -> IO ()
exec (Prog d s) = do
  let initStore = DataMap.fromList [(loopLocation, loopNormal)]
  let initEnv = DataMap.empty
  result <- runExceptT $ flip runStateT initStore $ flip runReaderT initEnv $ execStmt (SBlock d s)
  case result of
    Left err -> hPutStrLn stderr $ "Runtime Error: " ++ err
    Right _ -> return ()
