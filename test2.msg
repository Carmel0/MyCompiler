Protocol CR:
Knowledges:{
    A : m;
    B : <pk(A).m>
}
Actions:{
[seq1] A->B (n1) : aenc{<n.m.pk(a)>}k(a,b);
[seq1] A->B (n1) : aenc{<n.m.pk(a)>}k(a,b)
}
Goals:{
[secrecy] m secret of <A.B>;
[secrecy] m secret of <A.B>;
[weakB] B non-injectively agrees with A on m
}
end