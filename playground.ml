
type list a = 
| Nil 
| Cons of (a * (list a))

type option a = 
  | None
  | Some of a

let rec listbase n = 
  ( match n with 
      0 -> Nil
    | _ -> Cons (n, (listbase (n + (-1))))) in

let hd l = 
  ( match l with 
      Nil -> None
    | Cons (x, xs) -> Some(x)) in

let rec map f l = 
 ( match l with 
    Nil -> Nil
  | Cons (x, xs) -> Cons ( (f x), (map f xs))) in 
(map (fun a -> (a*a)) (listbase 10))