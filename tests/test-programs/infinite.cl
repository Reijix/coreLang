cons a b cc cn = cc a b;
nil cc cn = cn;
hd list = list K abort;
tl list = list K1 abort;
abort = abort;

infinite x = cons x (infinite x);
main = hd (tl (infinite 4))