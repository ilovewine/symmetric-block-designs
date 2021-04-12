:- module(set_subset,[subset_of_size/3,subsets/3,block_pairs/2,set_of_length/2]).

set_of_length(N,Set) :- setof(X,between(1,X,N),Set).

subset_of_size(N,K,Subset) :-
    length(Block,K),
    Block ins 1..N,
    chain(Block,#<),
    label(Block),
    sort(Block,Subset).

subsets(N,K,Family) :- setof(Subset,subset_of_size(N,K,Subset),Family).

block_pairs(Block,Pairs) :- setof(Pair,block_pair(Pair,Block),Pairs).
block_pair([P1,P2],Block) :-
    member(P1,Block),
    member(P2,Block),
    P1 < P2.