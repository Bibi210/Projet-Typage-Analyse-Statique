open Lexing

type position =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  }

(*For error messages*)
let err msg pos =
  Printf.eprintf
    "Error on line %d col %d: %s.\n"
    pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol)
    msg;
  exit 1
;;
