id x = x;
main = letrec
         num = twice id 3
       in id num