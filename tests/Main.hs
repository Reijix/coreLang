module Main where

import Mark4Test ( Mark4Test(Mark4Test) )
import TestUtils (runTest)

-- source: https://functional.works-hub.com/learn/basic-unit-testing-in-haskell-using-hunit-and-cabal-29e47

main :: IO ()
main = do
    runTest Mark4Test