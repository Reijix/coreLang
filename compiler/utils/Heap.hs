module Heap (Heap, Addr, hInitial, hAlloc, hUpdate, hFree, hLookup, hAddresses, 
             hSize, hNull, hIsNull, showAddr, heapStatsNumAllocs, heapStatsNumUpdates, heapStatsNumFrees, hAllocDBG, showHeap) where

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
heapStatsNumAllocs :: Heap a -> Int
heapStatsNumUpdates :: Heap a -> Int
heapStatsNumFrees :: Heap a -> Int


-- implementation
type Heap a = (Int, [Int], Assoc Addr a, HeapStats)
type Addr = Int
type HeapStats = (Int, Int, Int)

hInitial                                  = (0, [1..], [], initialStats)
    where initialStats = (0, 0, 0)
hAlloc     (size, next:free, cts, hs) n       = ((size + 1, free, (next, n) : cts, heapStatsIncAllocs hs), next)
hAlloc     _ _                                = error "hAlloc failed, no free space left!"
hUpdate    (size, free, cts, hs)      a n     = (size, free, (a,n) : remove cts a, heapStatsIncUpdates hs)
-- TODO maybe change hUpdate....
hFree      (size, free, cts, hs)      a       = (size - 1, a:free, remove cts a, heapStatsIncFrees hs)
hLookup    (size, free, cts, hs)      a       = aLookup cts a (error ("can't find node " ++ showAddr a ++ " in heap"))
hAddresses (size, free, cts, hs)              = [addr | (addr, node) <- cts]
hSize      (size, free, cts, hs)              = size
hNull                                     = 0
hIsNull                                   = (==) 0
showAddr a                                = "#" ++ show a
heapStatsNumAllocs (_, _, _, hs) = let (allocs, _, _) = hs in allocs
heapStatsNumUpdates (_, _, _, hs) = let (_, updates, _) = hs in updates
heapStatsNumFrees (_, _, _, hs) = let (_, _, frees) = hs in frees

hAllocDBG :: Heap a -> a -> Heap a
hAllocDBG h n = fst (hAlloc h n)

showHeap :: Show a => Heap a -> String
showHeap (_, _, cts, _) = show cts

-- helpers
remove :: Assoc Int a -> Int -> Assoc Int a
remove [] a                      = [] -- error ("Attempt to update or free nonexistent address #" ++ show a)
-- this change removes all occurences...
remove ((a', n):cts) a | a == a' = remove cts a
remove ((a', n):cts) a           = (a', n) : remove cts a
heapStatsIncAllocs :: HeapStats -> HeapStats
heapStatsIncAllocs (allocs, updates, frees) = (allocs + 1, updates, frees)
heapStatsIncUpdates :: HeapStats -> HeapStats
heapStatsIncUpdates (allocs, updates, frees) = (allocs, updates + 1, frees)
heapStatsIncFrees :: HeapStats -> HeapStats
heapStatsIncFrees (allocs, updates, frees) = (allocs, updates, frees + 1)

