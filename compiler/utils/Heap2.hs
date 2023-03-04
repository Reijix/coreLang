module Heap2 where

import Data.IntMap.Strict
import Prelude hiding (lookup, null)
import Data.Maybe

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
type Heap a = (IntMap a, Int)
type Addr = Int

hInitial = (empty, 1)
hAlloc (heap, next) n = ((insert next n heap, next + 1), next)
hUpdate (heap, next) a n = (insert a n heap, next)
hFree      (heap, next)      a       = (delete a heap, next)
hLookup    (heap, next)      a       = fromJust $ lookup a heap
hAddresses = keys . fst
hSize = size . fst
hNull                                         = 0
hIsNull                                       = (==) 0
showAddr a                                    = "#" ++ show a

