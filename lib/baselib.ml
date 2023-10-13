open Ast

type operator =
  { name : string
  ; otype : etype
  ; func : expr list -> expr
  }
