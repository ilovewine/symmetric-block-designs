:- module(dual_plane,[dual_plane/1]).
:- use_module(symmetric_bibd).

dual_plane(K) :-
	K1 is K+2,
	N is 1+(K1*(K1-1))/2,
	bibd(N,K1,2).
