sum xs = case xs of
        <1> -> 0;
        <2> y ys -> y + sum ys;
main = sum (cons 5 (cons 10 nil))