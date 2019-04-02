%token <string> IDENT
%token <string> STRING
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
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
%token EOF

%start <Json.value option> prog

%%
(* part 1 *)
prog:
  | m = message; EOF { Some m }
  | EOF       { None   } ;

message: 
  | id=IDENT { `Var id }
  | v1=message;PERIOD;v2=message{ `Concat (v1,v2)}
  | PK;LEFT_BRACK;rlnm=IDENT;RIGHT_BRACK { `Pk rlnm }
  | SK;LEFT_BRACK;rlnm=IDENT;RIGHT_BRACK { `Sk rlnm }
  | K;LEFT_BRACK;rlnm1=IDENT;COMMA;rlnm2=IDENT;RIGHT_BRACK { `K (rlnm1,rlnm2)}
  | HASHCON;LEFT_BRACK;v=message;RIGHT_BRACK {`Hash v}
  | AENC;LEFT_BRACE;v1=message;RIGHT_BRACE;v2=message {`Aenc (v1,v2)}
  | SENC;LEFT_BRACE;v1=message;RIGHT_BRACE;v2=message {`Senc (v1,v2)} 
  | LEFT_ANGLEBARCK;v1=message;PERIOD;v2=message;RIGHT_ANGLEBARCK { `Concat (v1,v2)}
  | LEFT_BRACK;v=message;RIGHT_BRACK { v }
  ;