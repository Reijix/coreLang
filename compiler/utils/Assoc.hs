module Assoc (Assoc, aLookup, aDomain, aRange, aEmpty) where

-- interface
aLookup :: (Eq a) => Assoc a b -> a -> b -> b
aDomain :: Assoc a b -> [a]
aRange :: Assoc a b -> [b]
aEmpty :: Assoc a b


-- implementation
type Assoc a b = [(a,b)]
aLookup [] k' def = def
aLookup ((k,v):bs) k' def | k == k' = v
aLookup ((k,v):bs) k' def = aLookup bs k' def

aDomain alist = [key | (key,val) <- alist]

aRange alist = [val | (key,val) <- alist]

aEmpty =  []