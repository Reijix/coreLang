module PrettyPrint (pprProgram, pprExpr, pprDefn, pprDefns) where

import Syntax
import ISeq
    ( ISeq, iConcat, iInterleave, iStr, iAppend, iNewline, iIndent )

{-
 - Operator precedence:
 - *, /       ; level 0
 - -, +       ; level 1
 - <, <=, ... ; level 2
 - &          ; level 3
 - |          ; level 4
 - -}

wrapParen :: Bool -> ISeq -> ISeq
wrapParen b x = if b then iConcat [iStr "(", x, iStr ")"] else x

pprExpr :: Int -> CoreExpr -> ISeq
pprExpr prec (EAp (EAp (EVar "+") e1) e2) = wrapParen (prec < 1) (iConcat [pprExpr 1 e1, iStr " + ", pprExpr 1 e2])
pprExpr prec (EAp (EAp (EVar "-") e1) e2) = wrapParen (prec < 1) (iConcat [pprExpr 1 e1, iStr " - ", pprExpr 0 e2])
pprExpr prec (EAp (EAp (EVar "*") e1) e2) = iConcat [pprExpr 0 e1, iStr " * ", pprExpr 0 e2]
pprExpr prec (EAp (EAp (EVar "/") e1) e2) = iConcat [pprExpr 0 e1, iStr " / ", pprExpr 0 e2]
pprExpr prec (EAp (EAp (EVar "<") e1) e2) = iConcat [pprExpr 2 e1, iStr " < ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar ">") e1) e2) = iConcat [pprExpr 2 e1, iStr " > ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "<=") e1) e2) = iConcat [pprExpr 2 e1, iStr " <= ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "==") e1) e2) = iConcat [pprExpr 2 e1, iStr " == ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "~=") e1) e2) = iConcat [pprExpr 2 e1, iStr " ~= ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar ">=") e1) e2) = iConcat [pprExpr 2 e1, iStr " >= ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "&") e1) e2) = iConcat [pprExpr 3 e1, iStr " & ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "|") e1) e2) = wrapParen (prec < 4) (iConcat [iStr "(", pprExpr 4 e1, iStr " | ", pprExpr 3 e2, iStr ")"])
pprExpr prec (EAp e1 e2) = pprExpr 10 e1 `iAppend` iStr " " `iAppend` pprExpr 10 e2
pprExpr prec (ELet isrec defns expr) =
  iIndent $
    iConcat
      [ iStr keyword,
        iStr " ",
        pprDefns defns,
        iNewline,
        iStr "in ",
        pprExpr 10 expr
      ]
  where
    keyword = if isrec then "letrec" else "let"
pprExpr prec (ECase expr alters) =
  iIndent $
    iConcat
      [ iStr "case ",
        pprExpr 10 expr,
        iStr " of",
        iNewline,
        iInterleave (iStr ";" `iAppend` iNewline) (map (iIndent . pprAlter) alters)
      ]
  where
    pprAlter :: Alter Name -> ISeq
    pprAlter (id, args, expr) =
      iConcat
        [ iStr "<",
          iStr (show id),
          iStr "> ",
          iConcat (map (\a -> iStr a `iAppend` iStr " ") args),
          iStr "-> ",
          pprExpr 10 expr
        ]
pprExpr prec (ELam params expr) = iConcat [iStr "\\", iInterleave (iStr " ") (map iStr params), iStr ". ", pprExpr 10 expr]
pprExpr prec (ENum n) = iStr (show n)
pprExpr prec (EVar v) = iStr v
pprExpr prec (EConstr n1 n2) = iConcat [iStr "Pack{", iStr (show n1), iStr ",", iStr (show n2), iStr "}"]

pprDefns :: [(Name, CoreExpr)] -> ISeq
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (Name, CoreExpr) -> ISeq
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr 10 expr)]

pprProgram :: CoreProgram -> ISeq
pprProgram scDefns = iInterleave (iStr ";" `iAppend` iNewline) (map pprScDefn scDefns)
  where
    pprScDefn :: CoreScDefn -> ISeq
    pprScDefn (name, params, expr) = iConcat [iStr name, iStr " ", iConcat (map (\p -> iStr p `iAppend` iStr " ") params), iStr "= ", pprExpr 10 expr]
