type option a = 
| None 
| Some of a

type list a = 
  Nil 
| Cons of (a * (list a))

let a = Some(()) in 
(match a with 
| Some(_) -> Cons((fun a -> a), Nil)
| Some(()) -> Nil
| None -> Nil )