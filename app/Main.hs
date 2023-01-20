module Main where

import PrettyPrint
import Syntax

atomExpr :: CoreExpr
atomExpr = EVar "test"

nonAtomExpr :: CoreExpr
nonAtomExpr = EAp (ENum 5) (ENum 10)

lamExpr :: CoreExpr
lamExpr = ELam ["x", "y", "z"] (EAp (EVar "x") (EVar "y"))

letExpr :: CoreExpr
letExpr = ELet recursive [("f", EAp (EVar "x") (EVar "y")), ("double", EAp (EAp (EVar "+") (EVar "x")) (EVar "x"))] (EAp (EVar "f") (EVar "z"))

main :: IO ()
main = do
  print (EVar "test" :: CoreExpr)
  print (isAtomicExpr nonAtomExpr)
  print (isAtomicExpr atomExpr)
  print (iDisplay (pprExpr 10 nonAtomExpr :: ISeqRep))
  putStrLn (iDisplay (pprExpr 10 lamExpr :: ISeqRep))
  putStrLn (iDisplay (pprExpr 10 letExpr :: ISeqRep))
