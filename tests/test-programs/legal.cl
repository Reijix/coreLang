map f xs = case xs of
    <1> -> nil;
    <2> p ps -> cons (f p) (map f ps);
prefix p xs = map (cons p) xs;
main = prefix 5 (cons 1 nil)