
type 'a list = 
| Nil 
| Cons of ('a * ( 'a list))


let hd l = 
   match l with 
    | Cons (x, xs) -> x 
    | Nil -> 0 ;;


let l = ref (Nil) in 
let toput = Cons(fun a -> a,Nil) in
let a = (l put toput) in ((hd !l) + 2)