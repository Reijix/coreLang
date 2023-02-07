module Mark4Test where

import Test.HUnit
import Main
import Mark4
import Parser
import TestUtils
import qualified System.Exit as Exit
import Data.Either (fromLeft)

-- source: https://functional.works-hub.com/learn/basic-unit-testing-in-haskell-using-hunit-and-cabal-29e47

tests = 
 -- (<program>, <expected>)
    [
    ("negate.cl", -3),
    ("negate2.cl", 3),
    ("arithTest1.cl", 10),
    ("arithTest2.cl", 25),
    ("arithTest3.cl", -1)
    ]


doTest :: String -> Int -> Test
doTest file expected = TestCase (assertEqual ("should return " ++ show expected) expected (runProgram file))

runProgram :: String -> Int
runProgram file = extractResult $ Mark4.eval $ Mark4.compile $ fromLeft [] $ parse file

testList :: Test
testList = TestList tl
    where
        tl = map (\(prog, expected) -> TestLabel prog (doTest prog expected)) tests

main :: IO ()
main = do
    result <- runTestTT testList
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess