:- module(symmetric_bibd,[bibd/3]).
:- use_module(library(clpfd)).

bibd(N,K,R2) :-
	bibd(N,K,R2,BD),
	setof(X,between(1,N,X),Set),
	write_list(Set),
	writeln_list(BD), !.
bibd(_,_,_) :- writeln('Such 2-BIBD does not exist.').

write_list([El|[]]) :- writeln(El).
write_list([El|List]) :- write(El), tab(1), write_list(List).

writeln_list([]).
writeln_list([El|List]) :- write_list(El), writeln_list(List).

bibd(N,K,R2,BD) :- subsets(N,2,Pairs), map_pairs(Pairs,Occurrences), choose_blocks(N,K,R2,Occurrences,Pairs,[],BD).
choose_blocks(N,_,R2,Occurrences,_,BD,BD) :- length(BD,N), nth1(_,Occurrences,occurrence(_,R2)), !.
choose_blocks(N,K,R2,Occurrences,[Pair|Pairs],Temp,BD) :-
	Pair = [P1,P2],
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence < R2,
	Remainder is K - 2,
	length(RemainingElements,Remainder),
	LowerBoundary is P2+1,
	RemainingElements ins LowerBoundary..N,
	all_distinct(RemainingElements),
	label(RemainingElements),
	Block = [P1,P2|RemainingElements],
	map_occurrences(Occurrences,R2,Block,Occurrences1),
	Temp1 = [Block|Temp],
	choose_blocks(N,K,R2,Occurrences1,[Pair|Pairs],Temp1,BD).
choose_blocks(N,K,R2,Occurrences,[Pair|Pairs],Temp,BD) :-
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence = R2,
	choose_blocks(N,K,R2,Occurrences,Pairs,Temp,BD).

map_pairs(Pairs,Occurrences) :- map_pairs(Pairs,[],Occurrences).
map_pairs([],Occurrences,Occurrences) :- !.
map_pairs([Pair|Pairs],Temp,Occurrences) :-
	Occurrence = occurrence(Pair,0),
	Temp1 = [Occurrence|Temp],
	map_pairs(Pairs,Temp1,Occurrences).

get_occurrence(Pair,Occurrences,Result) :- nth1(_,Occurrences,occurrence(Pair,Result)).

map_occurrences(Occurrences,R2,Block,Result) :- block_pairs(Block,Pairs), map_pairs_occurrences(Pairs,R2,Occurrences,Result).
map_pairs_occurrences([],_,Result,Result) :- !.
map_pairs_occurrences([Pair|Pairs],R2,Occurrences,Result) :-
	nth1(_,Occurrences,occurrence(Pair,Occurrence)),
	Occurrence1 is Occurrence+1,
	Occurrence1 =< R2,
	select(occurrence(Pair,Occurrence),Occurrences,occurrence(Pair,Occurrence1),Occurrences1),
	map_pairs_occurrences(Pairs,R2,Occurrences1,Result).

set_of_size(N,K,Subset) :-
    length(Block,K),
    Block ins 1..N,
    all_distinct(Block),
    label(Block),
    sort(Block,Subset).

subsets(N,K,Family) :- setof(Subset,set_of_size(N,K,Subset),Family).

block_pairs(Block,Pairs) :- setof(Pair,block_pair(Pair,Block),Pairs).
block_pair([P1,P2],Block) :-
    member(P1,Block),
    member(P2,Block),
    P1 < P2.

% [[1,2,3],[3,4,5],[5,6,1],[2,4,6],[1,7,4],[3,7,6],[2,7,5]]