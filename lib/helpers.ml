type position =
  { start_pos : Lexing.position
  ; end_pos : Lexing.position
  }

let dummy_position = { start_pos = Lexing.dummy_pos; end_pos = Lexing.dummy_pos }
let counter = ref 0

let symbolGenerator name =
  incr counter;
  Printf.sprintf "!%s_%d" name !counter
;;

let instanceGenerator name =
  incr counter;
  Printf.sprintf "instance_%d:%s" !counter name 

let getNameFromSymbol symbol =
  let symbol = String.sub symbol 1 (String.length symbol - 1) in
  let index = String.index_opt symbol '_' in
  match index with
  | Some i -> String.sub symbol 0 i
  | None -> symbol
;;

let resetSymbolGenerator () = counter := 0
