module TExec where

import System.IO
import qualified Data.Map as DataMap
import qualified Data.Set as DataSet
import Control.Monad.Except
import Control.Monad.Reader

import AbsGrammar
import TCheck
import TUtil
import Util

-- Special identifier to hold what is the currently executed function type
funcIdent :: Ident
funcIdent = Ident "func"

-- Special identifier to hold that we're currently in a loop
loopIdent :: Ident
loopIdent = Ident "while"

ensureType :: Type -> Exp -> TypeCheck ()
ensureType t e = do
  et <- evalExpType e
  unless (canAssign t et) $ throwError $
    "Cannot assign " ++ (show et) ++ " to " ++ (show t)

ensureInts :: [Exp] -> TypeCheck ()
ensureInts es = mapM_ (ensureType TInt) es

ensureBools :: [Exp] -> TypeCheck ()
ensureBools es = mapM_ (ensureType TBool) es

evalExpType :: Exp -> TypeCheck Type
evalExpType (ECall c) = case c of
  (CFun f es) -> do
    env <- ask
    t <- mustGet env f " was not declared in this scope."
    (args, ret) <- case t of
      (TFunc a r) -> return (a, r)
      _ -> throwError $ show f ++ " is not a function."
    ts <- mapM evalExpType es
    unless (args == ts) $ throwError $ "Function parameters type mismatch."
    return ret
  (CMet _ _ _) -> throwError $ "Not implemented yet." -- TODO: implement
evalExpType (EVar x) = do
  env <- ask
  t <- mustGet env x " was not declared in this scope."
  return t
evalExpType (ELit l) = case l of
  LNon -> return TNone
  LInt _ -> return TInt
  LBool _ -> return TBool
  LStr _ -> return TString
  LErr _ -> return TError
  LArr es -> do
    if es == [] then return $ TArray TAny else do
      ts <- mapM evalExpType es
      let t = DataSet.fromList ts
      unless (DataSet.size t == 1) $ throwError "Types in the array mismatch."
      return $ TArray $ ts !! 0
  LTup es -> mapM evalExpType es >>= return . TTuple
evalExpType (ETimes e f) = ensureInts [e, f] >> return TInt
evalExpType (EDiv e f) = ensureInts [e, f] >> return TInt
evalExpType (EMod e f) = ensureInts [e, f] >> return TInt
evalExpType (EPlus e f) = ensureInts [e, f] >> return TInt
evalExpType (EMinus e f) = ensureInts [e, f] >> return TInt
evalExpType (EComp e _ f) = ensureInts [e, f] >> return TBool
evalExpType (EBool e _ f) = ensureBools [e, f] >> return TBool

execDeclType :: Decl -> TypeCheck () -> TypeCheck ()
execDeclType (DVar x t v) typeCheck = do
  unless (isDeclarableType t) $ throwError "A variable cannot be declared as type None/Any."
  ensureType t v
  local (DataMap.insert x t) typeCheck
execDeclType (DFunc f ps r s) typeCheck = do
  pts <- mapM paramType ps
  let ft = TFunc pts r
  let funEnv = local (DataMap.insert f ft)
  foldr execParam ( funEnv . local (DataMap.insert funcIdent r) $ execStmtType s) ps
  funEnv typeCheck
  where
    paramType :: Param -> TypeCheck Type
    paramType (PVal _ t) = return t
    execParam :: Param -> TypeCheck () -> TypeCheck ()
    execParam (PVal x t) typeCheck = do
      unless (isDeclarableType t) $ throwError "A variable cannot be declared as type None/Any."
      local (DataMap.insert x t) typeCheck

execStmtType :: Stmt -> TypeCheck ()
execStmtType (SBlock d s) = foldr execDeclType (execManyStmtType s) d
execStmtType (SAssign x e) = do
  env <- ask
  t <- mustGet env x " was not declared in this scope."
  ensureType t e
execStmtType (SUnpack xs e) = do
  env <- ask
  ts <- mapM (\x -> mustGet env x " was not declared in this scope.") xs
  ensureType (TTuple ts) e
execStmtType (SReturn e) = do
  env <- ask
  t <- case env DataMap.!? funcIdent of
    Nothing -> throwError $ "Return can only be called inside a function delcaration."
    Just v -> return v
  ensureType t e
execStmtType (SPrint _) = return ()
execStmtType (SIf b s es) = do
  ensureType TBool b
  execStmtType s
  mapM_ (\(EElif e s) -> execStmtType $ SIf e s []) (es)
execStmtType (SIfelse b s es e) = do
  ensureType TBool b
  execStmtType s
  mapM_ (\(EElif e s) -> execStmtType $ SIf e s []) (es)
  execStmtType e
execStmtType (SWhile b s) = do
  ensureType TBool b
  local (DataMap.insert loopIdent TNone) $ execStmtType s
execStmtType SBreak = do
  env <- ask
  case env DataMap.!? loopIdent of
    Nothing -> throwError $ "Break can only be called inside a loop."
    Just _ -> return ()
execStmtType SCont = do
  env <- ask
  case env DataMap.!? loopIdent of
    Nothing -> throwError $ "Continue can only be called inside a loop."
    Just _ -> return ()
execStmtType (SCall c) = evalExpType (ECall c) >> return ()

execManyStmtType :: [Stmt] -> TypeCheck ()
execManyStmtType ss = mapM_ execStmtType ss

execType :: Program -> IO ()
execType (Prog d s) = do
  let initEnv = DataMap.empty
  result <- runExceptT $ flip runReaderT initEnv $ execStmtType (SBlock d s)
  case result of
    Left err -> hPutStrLn stderr $ "Type Error: " ++ err
    Right _ -> return ()
