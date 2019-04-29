消息是{Na,A}pk(B)的时候cons是这样构造的
---1.前面把Na和A构造对应消息结构
	msg.msgType:=nonce;
	msg.noncePart:=Na;
	num1:=lookup(msg);
---2.然后把Na和A放到mPair的part1和part2然后结合成一个mpair的消息结构
    msg.msgType:=mpair;
	msg.mPairpart1:=num1;
	msg.mPairpart2:=num2;
	num1:=lookup(msg);
---3.最后看是pk类型的加密，然后把mpair放进num
    msg.CryptKey.encTyp:=PK;
	msg.CryptKey.ag:=B;
	msg.CryptMsg:=num1;
	put "here4";
	num:=lookup(msg);
--------现在问题是两个，首先如果是超过两个的时候怎么构造-----
例如消息是{Na,A,B}pk(B)的时候
方案1是把和Na，A按上面步骤1，2用part1，part2先构造一个mapir，然后再把这个mpair作为part1，B作为part2构造一个新的mpair，然后再执行步骤3
方案2是mpair的part1，part2这样定义改成part12345...。有多少消息就用多少个part，组合成一条消息，然后执行步骤3.。。
方案1也就是嵌套成二叉树，方案2就是list。按你之前的意思应该是方案1.
现在问题就是方案1组合成的mpair

procedure cons<seq1>(<Na>:<NonceType>; <A>:<AgentIdType>; <B>:<AgentIdType>; Var msg:Message; Var num: indexType);
var <num1>,<num2>:indexType;
    ---k:KeyType;
begin
    clear msg;
	msg.msgType:=nonce;
	msg.noncePart:=Na;
	num1:=lookup(msg);
	put "here1";

	clear msg;
	msg.msgType:=agent;
	msg.ag:=A;
	put "here2";
	num2:=lookup(msg);
	clear msg;
	msg.msgType:=mpair;
	msg.mPairpart1:=num1;
	msg.mPairpart2:=num2;
	put "here3";
	num1:=lookup(msg);
	clear msg;
	msg.msgType:=encrypt;
	---k.encTyp:=PK;
	---k.ag:=B;
	---msg.k:=k;
	msg.CryptKey.encTyp:=PK;
	msg.CryptKey.ag:=B;
	msg.CryptMsg:=num1;
	put "here4";
	num:=lookup(msg);
end;
procedure cons_seq1(NA:NonceType;A:AgentIdType;C:AgentIdType;B:AgentIdType; Var msg:Message; Var num: indexType);
  var num1,num2:indexType;
  begin
  clear msg;
  msg.msgType:=nonce;
  msg.noncePart:=NA;
  num1:=lookup(msg);

  clear msg;
  msg.msgType:=agent;
  msg.ag:=A;
  num2:=lookup(msg);

  clear msg;
  msg.msgType:=mpair;
  msg.mPairpart1:=num1;
  msg.mPairpart2:=num2;
  num1:=lookup(msg);
  clear msg;
  msg.msgType:=agent;
  msg.ag:=C;
  num2:=lookup(msg);

  clear msg;
  msg.msgType:=mpair;
  msg.mPairpart1:=num1;
  msg.mPairpart2:=num2;
  num1:=lookup(msg);
  clear msg;
  msg.msgType:=encrypt;
  msg.CryptKey.encTyp:=PK;
  msg.CryptKey.ag:=B;
  msg.CryptMsg:=num1;
  num:=lookup(msg);
end;