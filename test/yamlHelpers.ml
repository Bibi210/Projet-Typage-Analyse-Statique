open Yaml

exception YamlParsingError of string

let printYaml t =
  match to_string t with
  | Ok x -> x
  | Error (`Msg e) -> raise (YamlParsingError ("Can't export to string : " ^ e))
;;

let rec getValue file k =
  match file with
  | [] -> raise (YamlParsingError ("Key : " ^ k ^ " do not exist in file"))
  | (string, value) :: tail -> if string = k then value else getValue tail k
;;

let getString = function
  | `String x -> x
  | _ -> raise (YamlParsingError "getString on non string value")
;;

let getBool = function
  | `Bool x -> x
  | _ -> raise (YamlParsingError "getBool on non Boolean value")
;;

let getList f = function
  | `A x -> List.map f x
  | _ -> raise (YamlParsingError "getList on non List value")
;;

let parse f =
  let f = open_in f in
  let s = really_input_string f (in_channel_length f) in
  close_in f;
  match of_string s with
  | Ok (`O x) -> x
  | Error (`Msg e) -> raise (YamlParsingError e)
  | _ -> raise (YamlParsingError "Not an assoc list")
;;
