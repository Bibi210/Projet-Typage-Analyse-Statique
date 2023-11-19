
type 'a list = 
| Nil 
| Cons of ('a * ( 'a list))

type 'a option = 
  | None
  | Some of 'a

let rec listbase n = 
  match n with 
      0 -> Nil
    | _ -> Cons (n, (listbase (n + (-1)))) ;;

let hd l = 
   match l with 
      Nil -> None
    | Cons (x, xs) -> Some(x) ;;

let rec map f l = 
  match l with 
    Nil -> Nil
  | Cons (x, xs) -> Cons ( (f x), (map f xs)) ;;
(map (fun a -> (a*a)) (listbase 10))