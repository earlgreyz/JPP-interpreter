

module AbsGrammar where

-- Haskell module generated by the BNF converter




newtype Ident = Ident String deriving (Eq, Ord, Show, Read)
newtype Error = Error String deriving (Eq, Ord, Show, Read)
data Program = Prog [Stmt]
  deriving (Eq, Ord, Show, Read)

data Boolean = BTrue | BFalse
  deriving (Eq, Ord, Show, Read)

data Map = MapKV Exp Exp
  deriving (Eq, Ord, Show, Read)

data Literal
    = LInt Integer
    | LBool Boolean
    | LStr String
    | LErr Error
    | LArr [Exp]
    | LTup [Exp]
    | LMap [Map]
  deriving (Eq, Ord, Show, Read)

data Comp = CLt | CGt | CLe | CGe | CEq
  deriving (Eq, Ord, Show, Read)

data BOp = BAnd | BOr
  deriving (Eq, Ord, Show, Read)

data Exp
    = ECall Call
    | EVar Ident
    | ELit Literal
    | ETimes Exp Exp
    | EDiv Exp Exp
    | EMod Exp Exp
    | EPlus Exp Exp
    | EMinus Exp Exp
    | EComp Exp Comp Exp
    | ENot Exp
    | EBool Exp BOp Exp
  deriving (Eq, Ord, Show, Read)

data Call = CFun Ident [Exp] | CMet Ident Ident [Exp]
  deriving (Eq, Ord, Show, Read)

data Type
    = TInt
    | TBool
    | TError
    | TString
    | TArray Type
    | TMap Type Type
    | TTuple [Type]
  deriving (Eq, Ord, Show, Read)

data Ret = RVoid | RType Type
  deriving (Eq, Ord, Show, Read)

data RVal = RExp Exp | RNone
  deriving (Eq, Ord, Show, Read)

data Param = PVal Ident Type
  deriving (Eq, Ord, Show, Read)

data Var = AVar Ident
  deriving (Eq, Ord, Show, Read)

data Decl = DVar Ident Type Exp | DFunc Ident [Param] Ret [Stmt]
  deriving (Eq, Ord, Show, Read)

data Elif = EElif Exp [Stmt]
  deriving (Eq, Ord, Show, Read)

data Else = EElse [Stmt]
  deriving (Eq, Ord, Show, Read)

data Stmt
    = SReturn RVal
    | SPrint Exp
    | SDecl Decl
    | SIf Exp [Stmt] [Elif]
    | SIfelse Exp [Stmt] [Elif] Else
    | SWhile Exp [Stmt]
    | SAssign [Var] Exp
    | SBreak
    | SCont
    | SCall Call
  deriving (Eq, Ord, Show, Read)

