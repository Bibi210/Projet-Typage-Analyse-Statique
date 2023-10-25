let fibo_term n =
  let rec fibo_term_aux n a b = if n = 0 then a else fibo_term_aux (n - 1) b (a + b) in
  fibo_term_aux n 0 1
;;

print_int (fibo_term 100);;