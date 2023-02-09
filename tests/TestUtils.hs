module TestUtils where
import Mark4
import Heap
import Test.HUnit.Base (Test (TestList))
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )


-- interface that all test files create an instance of
class TestRunner a where
    runTest :: a -> IO ()

-- globally used definitions
test_path :: String
test_path = "tests/test-programs/"


-- helper functions
extractResult :: [TIState] -> Int
extractResult states = result
    where
        (stack, _, heap, _, _) = last states
        result_addr = last stack
        result = case hLookup heap result_addr of
            (NNum n) -> n
            x -> error ("couldnt find result in heap??" ++ show x)

readTestFile :: String -> IO String
readTestFile path = do
    source <- openFile path ReadMode
    hGetContents source
