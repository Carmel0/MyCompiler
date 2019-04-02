type label = string;;
type roleName= string;;
type identifier = string;;
type value = [
  | `Null
  | `Var of identifier
  | `Str of roleName
  | `Concat of value*value
  | `Aenc of value*value
  | `Senc of value*value
  | `Hash of value
  | `Pk of roleName
  | `Sk of roleName
  | `K of roleName*roleName
]

(* part 1 *)
open Core.Std
let rec output_value outc = function
  | `Var id -> printf "~IDENT~!!"
  | `Str str -> printf "~STR~"
  | `Concat (ms1,ms2) -> printf "~Concat~"
  | `Aenc (ms1,ms2) -> printf "~Aenc~"
  | `Senc (ms1,ms2) -> printf "~Senc~"
  | `Hash msg -> printf "~hash~"
  | `Pk rolename -> printf "~Pk~"
  | `Sk rolename -> printf "~Sk~"
  | `K (r1,r2) -> printf "~K~"
  | `Null       -> output_string outc "null"