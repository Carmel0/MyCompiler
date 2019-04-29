Protocol CR:
Knowledges:{
    A : m;
    B : <pk(A).m>
}
Actions:{
[seq1] A->B (n1) : aenc{<nounce(a).A>}pk(B);
[seq2] B->A (n2) : aenc{<nounce(a).nounce(b)>}pk(A);
[seq3] A->B (n3) : aenc{nounce(b)}pk(B)
}
Goals:{
[secrecy] m secret of <A.B>;
[secrecy] m secret of <A.B>;
[weakB] B non-injectively agrees with A on m
}
end