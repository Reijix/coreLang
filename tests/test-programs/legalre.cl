main = f true;
f x = Pack{2,2} (g x) Pack{1,0};
g x = case x of <1> -> 1; <2> -> 2