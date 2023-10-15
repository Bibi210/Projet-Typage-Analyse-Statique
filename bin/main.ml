open ProjetTAS

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
    (* Print the AST *)
    print_endline "AST:";
    Prettyprinter.print_prog output;
    Typeur.infer output;
    let equations = Typeur.generateTypeEquations output in
    Prettyprinter.print_equation_list equations;
    (* Print the evaluation *)
    print_endline "Evaluation:";
    let _evaluated = Evaluator.betaReduce output in
    Prettyprinter.print_prog _evaluated;
  with
  | Lexer.LexingError s -> Prettyprinter.print_error s.msg s.pos
  | Parser.Error -> Prettyprinter.print_error "Syntax error" (Lexing.lexeme_start_p buf)
  | Typeur.TypingError s -> Prettyprinter.print_error_start_end s.message s.location
;;
