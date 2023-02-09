module UsefulFuns where

mapAccuml :: (a -> b -> (a, c)) -> a -> [b] -> (a, [c])
mapAccuml f acc [] = (acc, [])
mapAccuml f acc (x:xs) = (acc2, x':xs')
    where
        (acc1, x') = f acc x
        (acc2, xs') = mapAccuml f acc1 xs