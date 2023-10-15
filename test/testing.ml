open YamlHelpers
open ProjetTAS

let dirtyPrefix = "../../../"
let testDir = dirtyPrefix ^ "test/"
let directories = List.map (fun elem -> testDir ^ elem ^ "/") [ "SimpleLambda";"PCF" ]

type test =
  { filepath : string
  ; title : string
  ; description : string
  ; prog : string
  ; typing : bool
  ; io : string list
  }

type results =
  { equation : string
  ; evalTyping : bool
  ; ioResult : string list
  }

let mkTest f =
  let getValue = getValue (parse f) in
  { title = getString (getValue "titre")
  ; prog = getString (getValue "prog")
  ; description = getString (getValue "description")
  ; typing = getBool (getValue "typeur")
  ; io = getList getString (getValue "IO")
  ; filepath = f
  }
;;

type asserts =
  | IOElem of
      { index : int
      ; expected : string
      ; given : string
      }
  | Typing of
      { expected : bool
      ; given : bool
      }
  | IOLen of
      { expected : int
      ; given : int
      }

exception AssertFailure of asserts * results
exception ParserError of Lexing.position

let runTest test =
  let buf = Lexing.from_string test.prog in
  try
    let output = Parser.prog Lexer.token buf in
    let equation =
      Prettyprinter.string_of_equation_list (Typeur.generateProgTypeEquation output)
    in
    let _ = Typeur.infer output in
    let ioResult = [ Prettyprinter.string_of_prog (Evaluator.betaReduce output) ] in
    { equation; evalTyping = true; ioResult }
  with
  | Parser.Error -> raise (ParserError (Lexing.lexeme_start_p buf))
  | Typeur.TypingError e -> { equation = e.equation; evalTyping = false; ioResult = [] }
;;

let raise e res = raise (AssertFailure (e, res))

let assertTest test result =
  if test.typing != result.evalTyping
  then raise (Typing { expected = test.typing; given = result.evalTyping }) result;
  let expectedLen = List.length test.io in
  let givenLen = List.length result.ioResult in
  if expectedLen != givenLen
  then raise (IOLen { expected = expectedLen; given = givenLen }) result;
  List.iteri
    (fun i expected ->
      let given = List.nth result.ioResult i in
      if String.compare expected given != 0
      then raise (IOElem { index = i; expected; given }) result)
    test.io
;;

let showTest filePath =
  let test = mkTest filePath in
  Printf.printf "%s" test.title;
  try
    let result = runTest test in
    assertTest test result;
    print_endline " SUCCESS";
    true
  with
  | AssertFailure (e, r) ->
    print_newline ();
    Printf.eprintf "AssertFailure of : %s\n" test.title;
    Printf.eprintf "Description : %s\n" test.description;
    Printf.eprintf "Path := %s\n" test.filepath;
    Printf.eprintf "Equation :\n";
    Printf.eprintf "%s\n" r.equation;
    Printf.eprintf "***************************************\n";
    (match e with
     | IOElem { index; expected; given } ->
       Printf.eprintf
         "  IOAssert :\n    expected: %s  \n    given: %s at index: %d\n"
         expected
         given
         index
     | IOLen { expected; given } ->
       Printf.eprintf "  IOLen :\n   expected: %d  \n   given: %d\n" expected given
     | Typing { expected; given } ->
       Printf.eprintf "  Typing:\n   expected: %b \n   given: %b\n" expected given);
    false
  | Lexer.LexingError e ->
    Printf.eprintf "LexingFailure of : %s\n" test.description;
    Printf.eprintf "  Path := %s\n" test.filepath;
    Printf.eprintf "%s" (Prettyprinter.string_of_error e.msg e.pos);
    false
  | ParserError pos ->
    Printf.eprintf "ParsingFailure of : %s\n" test.description;
    Printf.eprintf "  Path := %s\n" test.filepath;
    Printf.eprintf "%s" (Prettyprinter.string_of_error "Parsing Error" pos);
    false
  | Evaluator.EvalError e ->
    Printf.eprintf "RuntimeFailure of : %s\n" test.description;
    Printf.eprintf "  Path := %s\n" test.filepath;
    Printf.eprintf "  %s\n" (Prettyprinter.string_of_error_start_end e.message e.location);
    false
  | Typeur.TypingError e ->
    Printf.eprintf "TypeFailure of : %s\n" test.description;
    Printf.eprintf "  Path := %s\n" test.filepath;
    Printf.eprintf "  %s\n" (Prettyprinter.string_of_error_start_end e.message e.location);
    false
;;

let getTestFiles d =
  let allFiles = List.map (fun f -> d ^ f) (Array.to_list (Sys.readdir d)) in
  List.filter (fun f -> Filename.extension f = ".yml") allFiles
;;

let runDirectory d =
  Printf.printf "Running Directory : %s\n" d;
  let success, fail =
    List.fold_left
      (fun (success, fail) elem ->
        if showTest elem then success + 1, fail else success, fail + 1)
      (0, 0)
      (getTestFiles d)
  in
  Printf.printf " Success = %d, Fail = %d\n" success fail
;;

let () = List.iter (fun d -> runDirectory d) directories
