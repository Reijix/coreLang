cons a b cc cn = cc a b;
nil cc cn = cn;
hd list = list K abort;
tl list = list K1 abort;
abort = abort;

length xs = xs length1 0;
length1 x xs = 1 + (length xs);

main = length (cons 3 (cons 3 (cons 3 nil)))