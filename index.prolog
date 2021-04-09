:- use_module(symmetric_bibd).
:- use_module(dual_plane).

symds(N,K,R2) :- bibd(N,K,R2).
symds(Order) :- dual_plane(Order).