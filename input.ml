let a = if true then fun a b : int -> b else fun x : ('b -> 'a) -> x in
print_int ((a (fun a -> a)) 3) 