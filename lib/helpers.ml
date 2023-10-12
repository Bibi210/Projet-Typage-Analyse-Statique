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
    msg
;;

(*For error messages*)
let errWithPosition msg position =
  Printf.eprintf
    "Error from line %d col %d to line %d col %d: \n%s.\n"
    position.start_pos.pos_lnum
    (position.start_pos.pos_cnum - position.start_pos.pos_bol)
    position.end_pos.pos_lnum
    (position.end_pos.pos_cnum - position.end_pos.pos_bol)
    msg
;;

let symbolGenerator =
  let counter = ref 0 in
  fun name ->
    incr counter;
    Printf.sprintf "%s_%d" name !counter
;;
