type variable =
  { id : string
  ; vpos : Helpers.position
  }

type expr =
  { epre : pre_expr
  ; epos : Helpers.position
  }

and pre_expr =
  | App of
      { func : expr
      ; carg : expr
      }
  | Lambda of
      { varg : variable
      ; body : expr
      }
  | Var of variable

type ctype = Nat

type etype =
  { tpre : pre_type
  ; tpos : Helpers.position
  }

and pre_type =
  | TLambda of
      { arg : etype
      ; body : etype
      }
  | TVar of string
  | Const of ctype

type prog = expr
