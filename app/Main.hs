module Main where

import Syntax
import PrettyPrint

atomExpr :: CoreExpr
atomExpr = EVar "test"

nonAtomExpr :: CoreExpr
nonAtomExpr = EAp (ENum 5) (ENum 10)

main :: IO ()
main = do
    print (EVar "test" :: CoreExpr)
    print (isAtomicExpr nonAtomExpr)
    print (isAtomicExpr atomExpr)
    print preludeDefs
