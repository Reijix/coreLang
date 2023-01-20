module PrettyPrint where

import Data.List
import Syntax

class ISeq t where
  iNil :: t
  iStr :: String -> t
  iAppend :: t -> t -> t
  iNewline :: t
  iIndent :: t -> t
  iDisplay :: t -> String

{-
 - Operator precedence:
 - *, /       ; level 0
 - -, +       ; level 1
 - <, <=, ... ; level 2
 - &          ; level 3
 - |          ; level 4
 - -}

pprExpr :: (ISeq b) => (Show b) => Int -> CoreExpr -> b
pprExpr prec (EAp (EAp (EVar "+") e1) e2) | prec < 1 = iConcat [iStr "(", pprExpr 1 e1, iStr " + ", pprExpr 1 e2, iStr ")"]
pprExpr prec (EAp (EAp (EVar "+") e1) e2) = iConcat [pprExpr 1 e1, iStr " + ", pprExpr 1 e2]
pprExpr prec (EAp (EAp (EVar "-") e1) e2) | prec < 1 = iConcat [iStr "(", pprExpr 1 e1, iStr " - ", pprExpr 2 e2, iStr ")"]
pprExpr prec (EAp (EAp (EVar "-") e1) e2) = iConcat [pprExpr 1 e1, iStr " - ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "*") e1) e2) = iConcat [pprExpr 0 e1, iStr " * ", pprExpr 0 e2]
pprExpr prec (EAp (EAp (EVar "/") e1) e2) = iConcat [pprExpr 0 e1, iStr " / ", pprExpr 0 e2]
pprExpr prec (EAp (EAp (EVar "<") e1) e2) = iConcat [pprExpr 2 e1, iStr " < ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar ">") e1) e2) = iConcat [pprExpr 2 e1, iStr " > ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "<=") e1) e2) = iConcat [pprExpr 2 e1, iStr " <= ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "==") e1) e2) = iConcat [pprExpr 2 e1, iStr " == ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "~=") e1) e2) = iConcat [pprExpr 2 e1, iStr " ~= ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar ">=") e1) e2) = iConcat [pprExpr 2 e1, iStr " >= ", pprExpr 2 e2]
pprExpr prec (EAp (EAp (EVar "&") e1) e2) = iConcat [pprExpr 3 e1, iStr " & ", pprExpr 3 e2]
pprExpr prec (EAp (EAp (EVar "|") e1) e2) | prec < 4 = iConcat [pprExpr 4 e1, iStr " | ", pprExpr 4 e2]
pprExpr prec (EAp (EAp (EVar "|") e1) e2) = iConcat [iStr "(", pprExpr 4 e1, iStr " | ", pprExpr 4 e2, iStr ")"]
pprExpr prec (EAp e1 e2) = pprExpr 10 e1 `iAppend` iStr " " `iAppend` pprExpr 10 e2
pprExpr prec (ELet isrec defns expr) =
  iConcat
    [ iStr keyword,
      iNewline,
      iStr " ",
      iIndent (pprDefns defns),
      iNewline,
      iStr "in ",
      pprExpr 10 expr
    ]
  where
    keyword = if isrec then "letrec" else "let"
pprExpr prec (ECase expr alters) = iConcat [iStr "case ", pprExpr 10 expr, iStr " of", iNewline, iConcat (map pprAlter alters)]
  where
    pprAlter :: (ISeq b) => (Show b) => Alter Name -> b
    pprAlter (id, args, expr) =
      iConcat
        [ iStr "<",
          iStr (show id),
          iStr "> ",
          iInterleave (iStr " ") (map iStr args),
          iStr " -> ",
          pprExpr 10 expr
        ]
pprExpr prec (ELam params expr) = iConcat [iStr "\\", iInterleave (iStr " ") (map iStr params), iStr ". ", pprExpr 10 expr]
pprExpr prec (ENum n) = iStr (show n)
pprExpr prec (EVar v) = iStr v
pprExpr prec (EConstr n1 n2) = iConcat [iStr "Pack{", iStr (show n1), iStr ",", iStr (show n2), iStr "}"]

pprDefns :: (ISeq b) => (Show b) => [(Name, CoreExpr)] -> b
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (ISeq b) => (Show b) => (Name, CoreExpr) -> b
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr 10 expr)]

pprProgram :: (ISeq b) => (Show b) => CoreProgram -> b
pprProgram scDefns = iInterleave iNewline (map pprScDefn scDefns)
  where
    pprScDefn :: (ISeq b) => (Show b) => CoreScDefn -> b
    pprScDefn (name, params, expr) = iConcat [iStr name, iStr " ", iInterleave (iStr " ") (map iStr params), iStr " = ", pprExpr 10 expr]

iConcat :: (ISeq b) => (Show b) => [b] -> b
iConcat = foldl iAppend iNil

iInterleave :: (ISeq b) => (Show b) => b -> [b] -> b
iInterleave x xs = iConcat (intersperse x xs)

-- IMPLEMENTATION:
data ISeqRep
  = INil
  | IStr [Char]
  | IAppend ISeqRep ISeqRep
  | IIndent ISeqRep
  | INewline
  deriving (Show)

instance ISeq ISeqRep where
  iNil = INil
  iStr str = iConcat2 (map (\c -> if c == "\n" then INewline else IStr c) (split2 str))
    where
      split2 str = case break (== '\n') str of
        (a, '\n' : b) -> a : "\n" : split2 b
        (a, "") -> [a]
      iConcat2 = foldl IAppend INil
  iAppend seq1 INil = seq1
  iAppend INil seq2 = seq2
  iAppend seq1 seq2 = IAppend seq1 seq2
  iNewline = INewline
  iIndent = IIndent
  iDisplay seq = flatten 0 [(seq, 0)]
    where
      flatten :: Int -> [(ISeqRep, Int)] -> String
      flatten col [] = ""
      flatten col (x : xs) = case x of
        (INil, ind) -> flatten col xs
        (IStr s, ind) -> s ++ flatten (col + length s) xs
        (IAppend seq1 seq2, ind) -> flatten col ((seq1, ind) : (seq2, ind) : xs)
        (INewline, ind) -> "\n" ++ spaces ind ++ flatten ind xs
        (IIndent seq, ind) -> flatten col ((seq, col) : xs)
      spaces :: Int -> String
      spaces num = replicate num ' '
