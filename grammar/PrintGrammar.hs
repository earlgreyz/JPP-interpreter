{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module PrintGrammar where

-- pretty-printer generated by the BNF converter

import AbsGrammar
import Data.Char


-- the top-level printing method
printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : "," :ts -> showString t . space "," . rend i ts
    t  : ")" :ts -> showString t . showChar ')' . rend i ts
    t  : "]" :ts -> showString t . showChar ']' . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t = showString t . (\s -> if null s then "" else (' ':s))

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- the printer class does the job
class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j<i then parenth else id


instance Print Integer where
  prt _ x = doc (shows x)


instance Print Double where
  prt _ x = doc (shows x)


instance Print Ident where
  prt _ (Ident i) = doc (showString ( i))


instance Print Error where
  prt _ (Error i) = doc (showString ( i))



instance Print Program where
  prt i e = case e of
    Prog stmts -> prPrec i 0 (concatD [prt 0 stmts])

instance Print Boolean where
  prt i e = case e of
    BTrue -> prPrec i 0 (concatD [doc (showString "true")])
    BFalse -> prPrec i 0 (concatD [doc (showString "false")])

instance Print Map where
  prt i e = case e of
    MapKV value1 value2 -> prPrec i 0 (concatD [prt 0 value1, doc (showString ":"), prt 0 value2])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Literal where
  prt i e = case e of
    LInt n -> prPrec i 0 (concatD [prt 0 n])
    LBool boolean -> prPrec i 0 (concatD [prt 0 boolean])
    LStr str -> prPrec i 0 (concatD [prt 0 str])
    LErr error -> prPrec i 0 (concatD [prt 0 error])
    LArr values -> prPrec i 0 (concatD [doc (showString "["), prt 0 values, doc (showString "]")])
    LTup values -> prPrec i 0 (concatD [doc (showString "<"), prt 0 values, doc (showString ">")])
    LMap maps -> prPrec i 0 (concatD [doc (showString "{"), prt 0 maps, doc (showString "}")])

instance Print Exp where
  prt i e = case e of
    ECall call -> prPrec i 2 (concatD [prt 0 call])
    EVar id -> prPrec i 2 (concatD [prt 0 id])
    EInt n -> prPrec i 2 (concatD [prt 0 n])
    ETimes exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "*"), prt 2 exp2])
    EDiv exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "/"), prt 2 exp2])
    EMod exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "%"), prt 2 exp2])
    EPlus exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "+"), prt 1 exp2])
    EMinus exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "-"), prt 1 exp2])

instance Print BExp where
  prt i e = case e of
    BCall call -> prPrec i 3 (concatD [prt 0 call])
    BVar id -> prPrec i 3 (concatD [prt 0 id])
    BBool boolean -> prPrec i 3 (concatD [prt 0 boolean])
    BLt exp1 exp2 -> prPrec i 2 (concatD [prt 0 exp1, doc (showString "<"), prt 0 exp2])
    BGt exp1 exp2 -> prPrec i 2 (concatD [prt 0 exp1, doc (showString ">"), prt 0 exp2])
    BLe exp1 exp2 -> prPrec i 2 (concatD [prt 0 exp1, doc (showString "<="), prt 0 exp2])
    BGe exp1 exp2 -> prPrec i 2 (concatD [prt 0 exp1, doc (showString ">="), prt 0 exp2])
    BEq exp1 exp2 -> prPrec i 2 (concatD [prt 0 exp1, doc (showString "=="), prt 0 exp2])
    BNot bexp -> prPrec i 1 (concatD [doc (showString "not"), prt 0 bexp])
    BAnd bexp1 bexp2 -> prPrec i 0 (concatD [prt 0 bexp1, doc (showString "and"), prt 1 bexp2])
    BOr bexp1 bexp2 -> prPrec i 0 (concatD [prt 0 bexp1, doc (showString "or"), prt 1 bexp2])

instance Print Type where
  prt i e = case e of
    TInt -> prPrec i 0 (concatD [doc (showString "int")])
    TBool -> prPrec i 0 (concatD [doc (showString "bool")])
    TError -> prPrec i 0 (concatD [doc (showString "error")])
    TVoid -> prPrec i 0 (concatD [doc (showString "void")])
    TArray type_ -> prPrec i 0 (concatD [doc (showString "["), prt 0 type_, doc (showString "]")])
    TMap type_1 type_2 -> prPrec i 0 (concatD [doc (showString "{"), prt 0 type_1, doc (showString ","), prt 0 type_2, doc (showString "}")])
    TTuple types -> prPrec i 0 (concatD [doc (showString "<"), prt 0 types, doc (showString ">")])
    TFunc type_1 type_2 -> prPrec i 0 (concatD [prt 0 type_1, doc (showString "->"), prt 0 type_2])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Ret where
  prt i e = case e of
    RVoid -> prPrec i 0 (concatD [doc (showString "void")])
    RType type_ -> prPrec i 0 (concatD [prt 0 type_])

instance Print Param where
  prt i e = case e of
    PVal id type_ -> prPrec i 0 (concatD [prt 0 id, prt 0 type_])
    PRef id type_ -> prPrec i 0 (concatD [prt 0 id, doc (showString "&"), prt 0 type_])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Func where
  prt i e = case e of
    FLambda params ret stmts -> prPrec i 0 (concatD [doc (showString "("), prt 0 params, doc (showString ")"), doc (showString "=>"), prt 0 ret, doc (showString "{"), prt 0 stmts, doc (showString "}")])
    FFunc id -> prPrec i 0 (concatD [prt 0 id])

instance Print Call where
  prt i e = case e of
    FCall id values -> prPrec i 0 (concatD [prt 0 id, doc (showString "("), prt 0 values, doc (showString ")")])

instance Print Value where
  prt i e = case e of
    VExp exp -> prPrec i 0 (concatD [prt 0 exp])
    VBExp bexp -> prPrec i 0 (concatD [prt 0 bexp])
    VLit literal -> prPrec i 0 (concatD [prt 0 literal])
    VCall func values -> prPrec i 0 (concatD [prt 0 func, doc (showString "("), prt 0 values, doc (showString ")")])
    VLambda params ret stmts -> prPrec i 0 (concatD [doc (showString "("), prt 0 params, doc (showString ")"), doc (showString "=>"), prt 0 ret, doc (showString "{"), prt 0 stmts, doc (showString "}")])
  prtList _ [] = (concatD [])
  prtList _ [x] = (concatD [prt 0 x])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ","), prt 0 xs])
instance Print Decl where
  prt i e = case e of
    DVar id type_ -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id, prt 0 type_])
    DVarI id type_ value -> prPrec i 0 (concatD [doc (showString "var"), prt 0 id, prt 0 type_, doc (showString "="), prt 0 value])
    DFunc id params ret stmts -> prPrec i 0 (concatD [doc (showString "func"), prt 0 id, doc (showString "("), prt 0 params, doc (showString ")"), doc (showString "->"), prt 0 ret, doc (showString "{"), prt 0 stmts, doc (showString "}")])

instance Print Stmt where
  prt i e = case e of
    SDecl decl -> prPrec i 0 (concatD [prt 0 decl])
    SAssign id value -> prPrec i 0 (concatD [prt 0 id, doc (showString "="), prt 0 value])
    SCall call -> prPrec i 0 (concatD [prt 0 call])
    SIf bexp stmts -> prPrec i 0 (concatD [doc (showString "if"), prt 0 bexp, doc (showString "{"), prt 0 stmts, doc (showString "}")])
    SIfelse bexp stmts1 stmts2 -> prPrec i 0 (concatD [doc (showString "if"), prt 0 bexp, doc (showString "{"), prt 0 stmts1, doc (showString "}"), doc (showString "else"), doc (showString "{"), prt 0 stmts2, doc (showString "}")])
    SWhile bexp stmts -> prPrec i 0 (concatD [doc (showString "while"), prt 0 bexp, doc (showString "{"), prt 0 stmts, doc (showString "}")])
    SFor id n1 n2 stmts -> prPrec i 0 (concatD [doc (showString "for"), prt 0 id, doc (showString "="), prt 0 n1, doc (showString "to"), prt 0 n2, doc (showString "{"), prt 0 stmts, doc (showString "}")])
    SReturn value -> prPrec i 0 (concatD [doc (showString "return"), prt 0 value])
    SDefer call -> prPrec i 0 (concatD [doc (showString "defer"), prt 0 call])
  prtList _ [] = (concatD [])
  prtList _ (x:xs) = (concatD [prt 0 x, doc (showString ";"), prt 0 xs])

