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

pprExpr :: (ISeq b) => (Show b) => CoreExpr -> b
pprExpr (EAp e1 e2) = pprExpr e1 `iAppend` iStr " " `iAppend` pprExpr e2
pprExpr (ELet isrec defns expr) =
  iConcat
    [ iStr keyword,
      iNewline,
      iStr " ",
      iIndent (pprDefns defns),
      iNewline,
      iStr "in ",
      pprExpr expr
    ]
  where
    keyword = if isrec then "letrec" else "let"
pprExpr (ECase expr alters) = iConcat [iStr "case ", pprExpr expr, iStr " of", iNewline, iConcat (map pprAlter alters)]
  where
    pprAlter :: (ISeq b) => (Show b) => Alter Name -> b
    pprAlter (id, args, expr) =
      iConcat
        [ iStr "<",
          iStr (show id),
          iStr "> ",
          iInterleave (iStr " ") (map iStr args),
          iStr " -> ",
          pprExpr expr
        ]
pprExpr (ELam params expr) = iConcat [iStr "\\", iInterleave (iStr " ") (map iStr params), iStr ". ", pprExpr expr]
pprExpr expr = pprAExpr expr

pprAExpr :: (ISeq b) => (Show b) => CoreExpr -> b
pprAExpr (ENum n) = iStr (show n)
pprAExpr (EVar v) = iStr v
pprAExpr (EConstr n1 n2) = iConcat [iStr "Pack{", iStr (show n1), iStr ",", iStr (show n2), iStr "}"]
pprAExpr expr = iConcat [iStr "( ", pprExpr expr, iStr " )"]

pprDefns :: (ISeq b) => (Show b) => [(Name, CoreExpr)] -> b
pprDefns defns = iInterleave sep (map pprDefn defns)
  where
    sep = iConcat [iStr ";", iNewline]

pprDefn :: (ISeq b) => (Show b) => (Name, CoreExpr) -> b
pprDefn (name, expr) = iConcat [iStr name, iStr " = ", iIndent (pprExpr expr)]

pprProgram :: (ISeq b) => (Show b) => CoreProgram -> b
pprProgram scDefns = iInterleave iNewline (map pprScDefn scDefns)
  where
    pprScDefn :: (ISeq b) => (Show b) => CoreScDefn -> b
    pprScDefn (name, params, expr) = iConcat [iStr name, iStr " ", iInterleave (iStr " ") (map iStr params), iStr " = ", pprExpr expr]

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
