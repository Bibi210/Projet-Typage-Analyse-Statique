type position =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  }

let dummy_position = { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }
let counter = ref 0

let symbolGenerator name =
  incr counter;
  Printf.sprintf "%s_%d" name !counter
;;

let getNameFromSymbol symbol =
  let index = String.index_opt symbol '_' in
  match index with
  | Some i -> String.sub symbol 0 i
  | None -> symbol
;;

let resetSymbolGenerator () = counter := 0
