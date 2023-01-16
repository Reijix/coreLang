module PrettyPrint where

import Syntax
import Data.List

class ISeq t where 
    iNil :: t
    iStr :: String -> t
    iAppend :: t -> t -> t
    iNewline :: t
    iIndent :: t -> t
    iDisplay :: t -> String

pprExpr :: (ISeq b) => (Show b) => CoreExpr -> b
pprExpr (ENum n) = iStr (show n)
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprExpr e2
pprExpr (ELet isrec defns expr) 
    = iConcat [ iStr keyword, iNewline,
                iStr " ", iIndent (pprDefns defns), iNewline,
                iStr "in ", pprExpr expr ]
    where keyword = if isrec then "letrec" else "let"
pprExpr (ECase expr alters) = iConcat [ iStr "case ", pprExpr expr, iStr " of", iNewline, iConcat (map pprAlter alters) ]
    where pprAlter :: (ISeq b) => (Show b) => Alter Name -> b
          pprAlter (id, args, expr) = iConcat [ iStr "<", iStr (show id), iStr "> ", iInterleave (iStr " ") (map iStr args), 
                                                iStr " -> ", pprExpr expr ]
-- TODO pprExpr (ELam )
--      pprAexpr
--      pprProgram
-- Page 15!!

pprDefns :: (ISeq b) => (Show b) => [(Name, CoreExpr)] -> b
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (ISeq b) => (Show b) => (Name, CoreExpr) -> b
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr)]

iConcat :: (ISeq b) => (Show b) => [b] -> b
iConcat = foldl iAppend iNil

iInterleave :: (ISeq b) => (Show b) => b -> [b] -> b
iInterleave x xs = iConcat (intersperse x xs)
