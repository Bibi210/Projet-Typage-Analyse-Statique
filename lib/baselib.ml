open Ast
module STDLIB = Map.Make (String)

type operator =
  { args_types : pre_type list
  ; return_type : pre_type
  ; func : pre_expr list -> pre_expr
  }

let stdpos = { Helpers.dummy_position with isSTD = true }
let pre_to_type pre = { tpre = pre; tpos = stdpos }

let std_lambda arg return =
  TApp
    { constructor = pre_to_type (TConst TLambda)
    ; args = [| pre_to_type arg; pre_to_type return |]
    }
;;

let std_binop arg1 arg2 return = std_lambda arg1 (std_lambda arg2 return)

let imposibleCrash x =
  Prettyprinter.print_pre_expr_list x;
  failwith "imposible crash at baselib"
;;

let int_of_bool i =
  match i with
  | true -> Const (Int 0)
  | false -> Const (Int 1)
;;

let bool_of_int i =
  match i with
  | 0 -> false
  | _ -> true
;;

let add =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> Const (Int (a + b))
        | x -> imposibleCrash x)
  }
;;

let sub =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> Const (Int (a - b))
        | x -> imposibleCrash x)
  }
;;

let mul =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> Const (Int (a * b))
        | x -> imposibleCrash x)
  }
;;

let div =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> Const (Int (a / b))
        | x -> imposibleCrash x)
  }
;;

let modulo =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> Const (Int (a mod b))
        | x -> imposibleCrash x)
  }
;;

let eq =
  (*TODO: We need an polymorphic equal *)
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> int_of_bool (a = b)
        | x -> imposibleCrash x)
  }
;;

let _not =
  { args_types = [ TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a) ] -> int_of_bool (not (bool_of_int a))
        | x -> imposibleCrash x)
  }
;;

let _or =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> int_of_bool (bool_of_int a || bool_of_int b)
        | x -> imposibleCrash x)
  }
;;

let lt =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> int_of_bool (a < b)
        | x -> imposibleCrash x)
  }
;;

let gt =
  { args_types = [ TConst TInt; TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a); Const (Int b) ] -> int_of_bool (a > b)
        | x -> imposibleCrash x)
  }
;;

let neg =
  { args_types = [ TConst TInt ]
  ; return_type = TConst TInt
  ; func =
      (function
        | [ Const (Int a) ] -> Const (Int (-a))
        | x -> imposibleCrash x)
  }
;;

let find_binop op =
  match op with
  | Add -> add
  | Mul -> mul
  | Mod -> sub
  | Div -> div
  | Lt -> lt
  | Gt -> gt
  | Eq -> eq
  | Or -> _or
;;

let find_unop op =
  match op with
  | Not -> _not
  | Neg -> neg
;;
