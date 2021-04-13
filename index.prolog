:- use_module(symmetric_bibd).

symds(N,K,R2) :- symmetric_bibd(N,K,R2).
symds(Order) :-
	K1 is Order+2,
    N is 1+(K1*(K1-1))/2,
    symmetric_bibd(N,K1,2).