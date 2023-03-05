module Mark4Test (Mark4Test (Mark4Test)) where

import TestUtils ( TestRunner(..), test_path, extractResult )
import Test.HUnit
    ( assertEqual, Counts(failures, errors), Test(..), runTestTT )
import Mark4 ( compile, eval )
import Data.Either (fromLeft)
import Parser (parse)
import qualified System.Exit as Exit
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )


testPrograms :: [(String, Int)]
testPrograms = 
 -- (<program>, <expected>)
    [
    ("negate.cl", -3),
    ("negate2.cl", 3),
    ("arithTest1.cl", 23),
    ("arithTest2.cl", 25),
    ("arithTest3.cl", -1)
    ]

data Mark4Test = Mark4Test

instance TestRunner Mark4Test where
    runTest _ = do
        -- build test list
        tl <- mapM (\(file, expected) -> (do
                                            source <- openFile (test_path ++ file) ReadMode
                                            prog <- hGetContents source
                                            return $ TestLabel file (doTest prog expected))
                    ) testPrograms
        -- run test list
        result <- runTestTT (TestList tl)
        -- check for success
        if errors result > 0 || failures result > 0 then Exit.exitFailure else Exit.exitSuccess

doTest :: String -> Int -> Test
doTest prog expected = TestCase (assertEqual ("should return " ++ show expected) expected (runProgram prog))

runProgram :: String -> Int
runProgram prog = extractResult $ Mark4.eval $ Mark4.compile $ fromLeft (error "parser error") $ parse prog