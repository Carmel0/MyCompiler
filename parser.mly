%token <string> IDENT
%token <string> USTR
%token <string> STRING
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_MIDBRACE
%token RIGHT_MIDBRACE
%token COMMA
%token PERIOD  
%left PERIOD 
%token PK
%token SK
%token K
%token LEFT_ANGLEBARCK
%token RIGHT_ANGLEBARCK
%token HASHCON
%token AENC
%token SENC
%token SENDTO
%token COLON
%token SEMICOLON
%token ACTIONS
%token EOF

%start <Json.action option> prog

%%
(* part 1 *)
prog:
  | act = actions; EOF { Some act }
  | EOF       { None   } ;

actions:
  | ACTIONS; actlist= action;  { actlist };

action:
  | LEFT_MIDBRACE; seq=IDENT; RIGHT_MIDBRACE ; r1=IDENT; SENDTO ; r2=IDENT;LEFT_BRACK;n=IDENT; RIGHT_BRACK;COLON;m=message {`Act (seq,r1,r2,n,m) }
  | LEFT_BRACE;acts = action_list; RIGHT_BRACE { `Actlist acts};

action_list:
   acts = separated_list(SEMICOLON, action)    { acts } ;

message: 
  | id=IDENT { `Var id}
  (*| v1=message;PERIOD;v2=message{ `Concat (v1,v2)}*)
  | PK;LEFT_BRACK;rlnm=IDENT;RIGHT_BRACK { `Pk rlnm }
  | SK;LEFT_BRACK;rlnm=IDENT;RIGHT_BRACK { `Sk rlnm }
  | K;LEFT_BRACK;rlnm1=IDENT;COMMA;rlnm2=IDENT;RIGHT_BRACK { `K (rlnm1,rlnm2)}
  | HASHCON;LEFT_BRACK;v=message;RIGHT_BRACK {`Hash v}
  | AENC;LEFT_BRACE;v1=message;RIGHT_BRACE;v2=message {`Aenc (v1,v2)}
  | SENC;LEFT_BRACE;v1=message;RIGHT_BRACE;v2=message {`Senc (v1,v2)} 
  | LEFT_ANGLEBARCK;msgs=message_list;RIGHT_ANGLEBARCK { `Concat msgs}
  | LEFT_BRACK;v=message;RIGHT_BRACK { v }
  ;

message_list:
  msgs = separated_list(PERIOD, message)    { msgs } ;
