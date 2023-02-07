module TestUtils where
import Mark4
import Heap

extractResult :: [TIState] -> Int
extractResult ((result:_, _, heap, _, _):_) = n
    where
        (NNum n) = hLookup heap result