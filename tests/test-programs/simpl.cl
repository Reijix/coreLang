f = 3;
g x y = let z = x in z;
h x = case (let y = x in y) of
    <1> -> 2;
    <2> -> 5
