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
  | `Secretgoal of label*message*roleName_list
  | `Agreegoal of label*roleName*roleName*message
  | `Goallist of goal list
  | `Null
];;
type knowledge = [
  | `knowledge of roleName*message
  | `knowledge_list of knowledge list
  | `Null
];;
type pocolcontext =[
  | `Pocol of knowledge*action*goal
  | `Null
];;
type protocols = [
  | `Protocol of label*pocolcontext
  | `Null
];;
type sign = 
  | Plus
  | Minus ;;
type act = sign * message ;;



(* part 1 *)
open Core.Std

let compileAct (seq,r1,r2,n,m) roleName = 
  if roleName=r1 then Some (Plus,m)
  else if roleName=r2 then Some (Minus,m)
  else None

(* let compileAct (seq,r1,r2,n,m) roleName = 
  match roleName with
  | r1 -> Some (Plus,m)
  | r2 -> Some (Minus,m)
  | _ -> None *)


let rec output_role outc = function
  | `RoleName role -> printf "role:%s" role
  | `roleName_list arr -> print_rolelist outc arr

and print_rolelist outc arr = 
  output_string outc "<";
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc " , ";
    output_role outc v) arr;
  output_string outc ">"

let rec head_message outc= function
  | `Var id ->printf "N%s:NonceType;" id
  | `Str str ->printf "%s:AgentIdType;" str
  | `Concat arr -> print_headmsglist outc arr
  | `Aenc(ms1,ms2) -> head_message outc ms1;head_message outc ms2
  | `Senc (ms1,ms2) ->  head_message outc ms1;head_message outc ms2
  | `Hash msg -> head_message outc msg
  | `Pk rolename ->printf "%s:AgentIdType;" rolename
  | `Sk rolename ->printf "%s:AgentIdType;" rolename
  | `K (r1,r2) -> printf "k(%s,%s)" r1 r2
  | `Null       -> printf "null"

and print_headmsglist outc arr = 
  List.iteri ~f:(fun i v ->
  head_message outc v;
  ) arr

let rec output_consmessage ?(varname="num1") outc = function
  | `Var id ->
  begin 
  printf "
  clear msg;
  msg.msgType:=nonce;
  msg.noncePart:=N%s;
  %s:=lookup(msg);\n" id varname
  end
  | `Str str ->
  begin
  printf "
  clear msg;
  msg.msgType:=agent;
  msg.ag:=%s;
  %s:=lookup(msg);\n" str varname
  end
  | `Concat arr -> print_consmsglist outc arr
  | `Aenc (ms1,ms2) -> output_consmessage outc ms1;output_consmessage outc ms2
  | `Senc (ms1,ms2) -> output_consmessage outc ms1;output_consmessage outc ms2
  | `Hash msg -> output_consmessage outc msg
  | `Pk rolename ->
  begin printf "
  clear msg;
  msg.msgType:=encrypt;
  msg.CryptKey.encTyp:=PK;
  msg.CryptKey.ag:=%s;
  msg.CryptMsg:=num1;
  num:=lookup(msg);\n" rolename
  end
  | `Sk rolename -> printf "sk(%s)" rolename
  | `K (r1,r2) -> printf "k(%s,%s)" r1 r2
  | `Null       -> output_string outc "null"

and print_consmsglist outc arr = 
  List.iteri ~f:(fun i v ->
  if i > 0 then
           output_consmessage outc v ~varname:"num2"
           else
           output_consmessage outc v;
  ) arr;
  output_string outc "
  clear msg;
  msg.msgType:=mpair;
  msg.mPairpart1:=num1;
  msg.mPairpart2:=num2;
  num1:=lookup(msg);"

let cons_message outc seq m =
  begin
  printf "
procedure cons_%s(%a Var msg:Message; Var num: indexType);
  var num1,num2:indexType;
  begin" seq head_message m ;
  output_consmessage outc m;
  printf "end;\n"
  end

let rec output_destructmessage ?(varname="num1") outc = function
  | `Var id ->
  printf "
  N%s:=msg%s.noncePart;\n" id varname
  | `Str str ->
  printf "
  %s:=msg%s.ag;;\n" str varname
  | `Concat arr -> print_desmsglist outc arr
  | `Aenc (ms1,ms2) -> output_destructmessage outc ms2;output_destructmessage outc ms1
  | `Senc (ms1,ms2) -> output_destructmessage outc ms2;output_destructmessage outc ms1
  | `Hash msg -> output_destructmessage outc msg
  | `Pk rolename ->
  begin printf "
  clear msg1;
  %s:=msg.CryptKey.ag;
  msg1:=msgs[msg.CryptMsg];\n" rolename
  end
  | `Sk rolename -> printf "sk(%s)" rolename
  | `K (r1,r2) -> printf "k(%s,%s)" r1 r2
  | `Null       -> output_string outc "null"

and print_desmsglist outc arr = 
  List.iteri ~f:(fun i v ->
  if i > 0 then
           begin
            printf "msgNum2:=msgs[msg1.mPairpart2];"; 
            output_destructmessage outc v ~varname:"num2"
           end
            else
           begin
            printf "msgNum1:=msgs[msg1.mPairpart1];"; 
            output_destructmessage outc v;
           end
  ) arr

let cons_message outc seq m =
  begin
  printf "
procedure cons_%s(%a Var msg:Message; Var num: indexType);
  var num1,num2:indexType;
  begin" seq head_message m ;
  output_consmessage outc m;
  printf "end;\n"
  end

let destruct_message outc seq m =
  begin
  printf "
procedure destruct_%s(msg:Message; Var %a);
  var msgNum1,msgNum2:Message;
  begin" seq head_message m ;
  output_destructmessage outc m;
  printf "end;\n"
  end

let rec output_action outc = function
  | `Null       -> output_string outc "null"
  | `Actlist arr  -> print_actionlist outc arr
  | `Act (seq,r1,r2,n,m) ->
  match compileAct (seq,r1,r2,n,m) "A" with
    | Some (Plus,m) -> printf "(+)"
    | Some (Minus,m) -> printf "(-)"
    | None -> printf "Empty"

and print_actionlist outc arr = 
  output_string outc "Actions{\n";
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc "\n-------\n";
  output_action outc v) arr;
  output_string outc "\n}"

let rec output_knowledge outc = function
  | `Null       -> output_string outc "null"
  | `knowledge_list arr  -> print_knowledgelist outc arr
  | `knowledge (r,m) -> printf "r:%s\nmessage:" r;output_consmessage outc m

and print_knowledgelist outc arr = 
  output_string outc "Knowledges{\n";
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc "\n-------\n";
  output_knowledge outc v) arr;
  output_string outc "\n}"

let rec output_goal outc = function
  | `Null       -> output_string outc "null"
  | `Goallist arr  -> print_goallist outc arr
  | `Agreegoal (seq,r1,r2,m) -> printf "Label:%s\nr1:%s\nr2:%s\nmessage:" seq r1 r2;output_consmessage outc m
  | `Secretgoal (seq,m,rlist) -> printf "Label:%s\nmessage:\nrlist:%a" seq output_role rlist;output_consmessage outc m

and print_goallist outc arr = 
  output_string outc "Goals{\n";
  List.iteri ~f:(fun i v ->
  if i > 0 then
    output_string outc "\n-------\n";
  output_goal outc v) arr;
  output_string outc "\n}"

let output_pocolcontext outc = function
  | `Null       -> output_string outc "null"
  | `Pocol (k,a,g)-> printf "%a\n----\n%a\n----\n%a" output_knowledge k output_action a output_goal g

  
let output_pocol outc = function
  | `Null       -> output_string outc "null"
  | `Protocol (n,p)  -> printf "Protocol %s:\n----\n%a\n END" n output_pocolcontext p