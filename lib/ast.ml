type variable =
  { id : string
  ; vpos : Helpers.position
  }

type ctype =
  | TNat
  | TInt

type etype =
  { tpre : pre_type
  ; tpos : Helpers.position
  }

and pre_type =
  | TLambda of
      { targ : etype
      ; tbody : etype
      }
  | TVar of string
  | TConst of ctype
  | TAny of
      { id : string
      ; polytype : etype
      }

type econst = Int of int

type bin_op =
  | Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq

type un_op =
  | Neg
  | Not

type expr =
  { epre : pre_expr
  ; epos : Helpers.position
  ; etyp_annotation : etype option
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
  | Var of string
  | Const of econst
  | If of
      { cond : expr
      ; tbranch : expr
      ; fbranch : expr
      }
  | Let of
      { varg : variable
      ; init : expr
      ; body : expr
      }
  | Fix of
      { varg : variable
      ; body : expr
      }

type prog = expr

type equation =
  { left : etype
  ; right : etype
  }
