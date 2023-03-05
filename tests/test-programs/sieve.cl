main = take 10 (sieve (from 2)) ;
from n = cons n (from (n+1)) ;
sieve xs = case xs of
            <1> -> nil ;
            <2> p ps -> cons p (sieve (filter (nonMultiple p) ps)) ;
filter predicate xs
        = case xs of
                <1> -> nil ;
                <2> p ps -> let rest = filter predicate ps
                            in
                            if (predicate p) (cons p rest) rest ;
nonMultiple p n = ((n/p)*p) ~= n ;
take n xs = if (n==0)
                nil
                (case xs of
                    <1> -> nil ;
                    <2> p ps -> cons p (take (n-1) ps))