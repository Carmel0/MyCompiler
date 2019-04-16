type label = string;;
type roleName= string;;
type identifier = string;;
type identifier_list=[
  | `Identifier of identifier
  | `Identifier_list of identifier list
];;
type roleName_list=[
  | `RoleName of roleName
  | `roleName_list of roleName_list list
];;
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
type goal = [
  | `Secretgoal of label*message*identifier_list
  | `Agreegoal of label*roleName*roleName*message
  | `Goallist of goal list
  | `Null
];;
type pocol =[
  | `Pocol of goal*action
  | `Null
];;

(* part 1 *)
open Core.Std
let rec output_role outc = function
  | `Identifier role -> printf "role:%s" role
  | `Identifier_list arr -> print_rolelist outc arr

and print_rolelist outc arr = 
  output_string outc "<";
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc " , ";
    output_role outc v) arr;
  output_string outc ">"

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

let rec output_action outc = function
  | `Null       -> output_string outc "null"
  | `Actlist arr  -> print_actionlist outc arr
  | `Act (seq,r1,r2,n,m) -> printf "Seq:%s\nr1:%s\nr2:%s\nnounce:%s\nmessage:%a" seq r1 r2 n output_message m

and print_actionlist outc arr = 
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc "\n-------\n";
  output_action outc v) arr

let rec output_goal outc = function
  | `Null       -> output_string outc "null"
  | `Goallist arr  -> print_goallist outc arr
  | `Agreegoal (seq,r1,r2,m) -> printf "Label:%s\nr1:%s\nr2:%s\nmessage:%a" seq r1 r2 output_message m
  | `Secretgoal (seq,m,rlist) -> printf "Label:%s\nmessage:%a\nrlist:%a" seq output_message m output_role rlist

and print_goallist outc arr = 
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc "\n-------\n";
  output_goal outc v) arr

let output_pocol outc = function
  | `Null       -> output_string outc "null"
  | `Pocol (g,a)-> printf "%a\n----\n%a" output_goal g output_action a