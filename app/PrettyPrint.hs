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

pprExpr :: (ISeq b) => CoreExpr -> b
pprExpr (ENum n) = iStr (show n)
pprExpr (EVar v) = iStr v
pprExpr (EAp e1 e2) = (pprExpr e1) `iAppend` (iStr " ") `iAppend` (pprExpr e2)
pprExpr (ELet isrec defns expr) 
    = iConcat [ iStr keyword, iNewline,
                iStr " ", iIndent (pprDefns defns), iNewline,
                iStr "in ", pprExpr expr ]
    where keyword = if isrec then "letrec" else "let"
-- TODO pprExpr (ECase )
--      pprExpr (ELam )
--      pprAexpr
--      pprProgram
-- Page 15!!

pprDefns :: (ISeq b) => [(Name, CoreExpr)] -> b
pprDefns defns = iInterleave sep (map pprDefn defns)
                 where sep = iConcat [ iStr ";", iNewline ]

pprDefn :: (ISeq b) => (Name, CoreExpr) -> b
pprDefn (name, expr) = iConcat [ iStr name, iStr " = ", iIndent (pprExpr expr)]

iConcat :: (ISeq b) => [b] -> b
iConcat bs = foldl iAppend iNil bs

iInterleave :: (ISeq b) => b -> [b] -> b
iInterleave x xs = iConcat $ intersperse x xs
