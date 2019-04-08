type label = string;;
type roleName= string;;
type identifier = string;;
(* type identifier_list=[
  | `Identifier of identifier
  | `Identifier_list of identifier_list list
];; *)
type message = [
  | `Null
  | `Var of identifier
  | `Str of roleName
  | `Concat of message list
  | `Aenc of message*message
  | `Senc of message*message
  | `Hash of message
  | `Pk of roleName
  | `Sk of roleName
  | `K of roleName*roleName
];;
type action = [
  | `Act of label*roleName*roleName*identifier*message
  | `Actlist of action list
  | `Null
];;


(* part 1 *)
open Core.Std
let rec output_message outc = function
  | `Var id -> printf "IDENT:%s" id
  | `Str str -> printf "STR:%s" str
  | `Concat arr -> print_msglist outc arr
  | `Aenc (ms1,ms2) -> printf "Aenc(%a)%a" output_message ms1 output_message ms2
  | `Senc (ms1,ms2) -> printf "Senc(%a)%a" output_message ms1 output_message ms2
  | `Hash msg -> printf "h(%a)" output_message msg
  | `Pk rolename -> printf "pk(%s)" rolename
  | `Sk rolename -> printf "sk(%s)" rolename
  | `K (r1,r2) -> printf "k(%s,%s)" r1 r2
  | `Null       -> output_string outc "null"

and print_msglist outc arr = 
  output_string outc "<";
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc " . ";
  output_message outc v) arr;
  output_string outc ">"

(* and let output_concatmessage outc obj = function
  printf "%s\"%s\": %a" output_message value; *)

let rec output_action outc = function
  | `Null       -> output_string outc "null"
  | `Actlist arr  -> print_actionlist outc arr
  | `Act (seq,r1,r2,n,m) -> printf "Seq:%s\nr1:%s\nr2:%s\nnounce:%s\nmessage:%a" seq r1 r2 n output_message m

and print_actionlist outc arr = 
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc "\n-------\n";
  output_action outc v) arr;