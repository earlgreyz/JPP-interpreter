module SkelGrammar where

-- Haskell module generated by the BNF converter

import AbsGrammar
import ErrM
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transError :: Error -> Result
transError x = case x of
  Error string -> failure x
transProgram :: Program -> Result
transProgram x = case x of
  Prog stmts -> failure x
transBoolean :: Boolean -> Result
transBoolean x = case x of
  BTrue -> failure x
  BFalse -> failure x
transMap :: Map -> Result
transMap x = case x of
  MapKV value1 value2 -> failure x
transLiteral :: Literal -> Result
transLiteral x = case x of
  LInt integer -> failure x
  LBool boolean -> failure x
  LStr string -> failure x
  LErr error -> failure x
  LArr values -> failure x
  LTup values -> failure x
  LMap maps -> failure x
transExp :: Exp -> Result
transExp x = case x of
  ECall call -> failure x
  EVar ident -> failure x
  EInt integer -> failure x
  ETimes exp1 exp2 -> failure x
  EDiv exp1 exp2 -> failure x
  EMod exp1 exp2 -> failure x
  EPlus exp1 exp2 -> failure x
  EMinus exp1 exp2 -> failure x
transBExp :: BExp -> Result
transBExp x = case x of
  BCall call -> failure x
  BVar ident -> failure x
  BBool boolean -> failure x
  BLt exp1 exp2 -> failure x
  BGt exp1 exp2 -> failure x
  BLe exp1 exp2 -> failure x
  BGe exp1 exp2 -> failure x
  BEq exp1 exp2 -> failure x
  BNot bexp -> failure x
  BAnd bexp1 bexp2 -> failure x
  BOr bexp1 bexp2 -> failure x
transType :: Type -> Result
transType x = case x of
  TInt -> failure x
  TBool -> failure x
  TError -> failure x
  TString -> failure x
  TArray type_ -> failure x
  TMap type_1 type_2 -> failure x
  TTuple types -> failure x
  TFunc type_1 type_2 -> failure x
transRet :: Ret -> Result
transRet x = case x of
  RVoid -> failure x
  RType type_ -> failure x
transParam :: Param -> Result
transParam x = case x of
  PVal ident type_ -> failure x
  PRef ident type_ -> failure x
transFunc :: Func -> Result
transFunc x = case x of
  FLambda params ret stmts -> failure x
  FFunc ident -> failure x
transCall :: Call -> Result
transCall x = case x of
  CFun ident values -> failure x
  CMet ident1 ident2 values -> failure x
transValue :: Value -> Result
transValue x = case x of
  VExp exp -> failure x
  VBExp bexp -> failure x
  VLit literal -> failure x
  VCall func values -> failure x
  VLambda params ret stmts -> failure x
transVar :: Var -> Result
transVar x = case x of
  VVar ident -> failure x
transDecl :: Decl -> Result
transDecl x = case x of
  DVar ident type_ -> failure x
  DVarI ident type_ value -> failure x
  DFunc ident params ret stmts -> failure x
transStmt :: Stmt -> Result
transStmt x = case x of
  SDecl decl -> failure x
  SAssign vars value -> failure x
  SCall call -> failure x
  SIf bexp stmts -> failure x
  SIfelse bexp stmts1 stmts2 -> failure x
  SWhile bexp stmts -> failure x
  SFor ident integer1 integer2 stmts -> failure x
  SReturn value -> failure x
  SDefer call -> failure x

