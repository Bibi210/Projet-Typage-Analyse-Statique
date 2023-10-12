

type position =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  }
;;

let symbolGenerator =
  let counter = ref 0 in
  fun name ->
    incr counter;
    Printf.sprintf "%s_%d" name !counter
;;
