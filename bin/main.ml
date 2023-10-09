open ProjetTAS
open Helpers

let () =
  if Array.length Sys.argv != 2
  then (
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0);
    exit 1);
  (*Open the file to compile*)
  let f = open_in Sys.argv.(1) in
  (* Tokenize the file *)
  let buf = Lexing.from_channel f in
  try
    let output = Parser.prog Lexer.token buf in
    close_in f;
    print_endline "Parsed AST:";
    Prettyprinter.fprintf_prog Format.std_formatter (Evaluator.betaReduce output)
  with
  | Lexer.LexingError s -> err s.msg s.pos
  | Parser.Error -> err "Syntax error" (Lexing.lexeme_start_p buf)
;;
