type list a = 
  Nil 
| Cons of (a * (list a))

let l  = ref Nil in 
let x = (l put Cons (fun x -> x,Nil)) in fun x -> let a = (fun c -> let y = x in y) in ((a 1) x)