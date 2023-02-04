module Heap (Heap, Addr, hInitial, hAlloc, hUpdate, hFree, hLookup, hAddresses, hSize, hNull, hIsNull, showAddr) where

import Assoc

-- interface
hInitial :: Heap a
hAlloc :: Heap a -> a -> (Heap a, Addr)
hUpdate :: Heap a -> Addr -> a -> Heap a
hFree :: Heap a -> Addr -> Heap a
hLookup :: Heap a -> Addr -> a
hAddresses :: Heap a -> [Addr]
hSize :: Heap a -> Int
hNull :: Addr
hIsNull :: Addr -> Bool
showAddr :: Addr -> String


-- implementation
type Heap a = (Int, [Int], Assoc Int a)
type Addr = Int

hInitial                                  = (0, [1..], [])
hAlloc     (size, next:free, cts) n       = ((size + 1, free, (next, n) : cts), next)
hUpdate    (size, free, cts)      a n     = (size, free, (a,n) : remove cts a)
hFree      (size, free, cts)      a       = (size - 1, a:free, remove cts a)
hLookup    (size, free, cts)      a       = aLookup cts a (error ("can't find node " ++ showAddr a ++ " in heap"))
hAddresses (size, free, cts)              = [addr | (addr, node) <- cts]
hSize      (size, free, cts)              = size
hNull                                     = 0
hIsNull                                   = (==) 0
showAddr a                                = "#" ++ show a

remove :: Assoc Int a -> Int -> Assoc Int a
remove [] a                      = error ("Attempt to update or free nonexistent address #" ++ show a)
remove ((a', n):cts) a | a == a' = cts
remove ((a', n):cts) a           = (a', n) : remove cts a

