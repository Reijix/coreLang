f x = let y = f 3 in g (let z = 4 in h z) y;
g x y = x;
h z = z;
main = f 5