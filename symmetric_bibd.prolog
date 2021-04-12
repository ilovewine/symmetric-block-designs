%:- module(symmetric_bibd,[symmetric_bibd/3,symmetric_bibd2/3]).
:- use_module(library(clpfd)).
:- use_module(set_subset).
:- use_module(write_list).

symmetric_bibd(N,K,R2) :-
	check_necessary_conditions(N,K,R2),
	symmetric_bibd(N,K,R2,BD),
	set_of_length(N,Set),
	write_list(Set),
	writeln_list(BD), !.
symmetric_bibd(_,_,_) :- writeln('Such symmetric 2-bibd does not exist.').

check_necessary_conditions(N,K,R2) :-
	R1 is R2*(N-1)/(K-1),
	R1 =:= K.

symmetric_bibd(N,K,R2,BD) :-
	subsets(N,2,Pairs),
	map_pairs(Pairs,Occurrences),
	set_of_length(K,InitialBlock),
	writeln(InitialBlock),
	choose_blocks(N,K,R2,Occurrences,Pairs,[InitialBlock],BD).
choose_blocks(N,_,R2,Occurrences,_,BD,BD) :- length(BD,N), check_occurrences(Occurrences,R2), !.
choose_blocks(N,K,R2,Occurrences,[Pair|Pairs],Temp,BD) :-
	Pair = [P1,P2],
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence < R2,
	Remainder is K - 2, !,
	create_block(Pair,Remainder,Occurrences,R2,RemainingElements),
	Block = [P1,P2|RemainingElements],
	\+ member(Block,Temp),
	map_occurrences(Occurrences,R2,Block,Occurrences1),
	sort([Block|Temp],Temp1),
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

check_occurrences([],_) :- !.
check_occurrences([Occurrence|Occurrences],R2) :-
	arg(2,Occurrence,R2),
	check_occurrences(Occurrences,R2).

get_occurrence(Pair,Occurrences,Result) :- nth1(_,Occurrences,occurrence(Pair,Result)).

map_occurrences(Occurrences,R2,Block,Result) :- block_pairs(Block,Pairs), map_pairs_occurrences(Pairs,R2,Occurrences,Result).
map_pairs_occurrences([],_,Result,Result) :- !.
map_pairs_occurrences([Pair|Pairs],R2,Occurrences,Result) :-
	nth1(_,Occurrences,occurrence(Pair,Occurrence)),
	Occurrence1 is Occurrence+1,
	Occurrence1 =< R2,
	select(occurrence(Pair,Occurrence),Occurrences,occurrence(Pair,Occurrence1),Occurrences1),
	map_pairs_occurrences(Pairs,R2,Occurrences1,Result).

create_block(Pair,Remainder,Occurrences,R2,RemainingElements) :- create_block(Pair,Remainder,Occurrences,R2,[],RemainingElements).
create_block(_,0,_,_,RemainingElements,RemainingElements) :- !.
create_block([P1,P2],Remainder,Occurrences,R2,Temp,RemainingElements) :-
	nth1(_,Occurrences,occurrence([P1,P3],Occurrence)),
	Occurrence < R2,
	occurrence_pairwise(P3,[P1,P2|Temp],Occurrences,R2),
	Temp1 = [P3|Temp],
	Remainder1 is Remainder - 1,
	create_block([P1,P2],Remainder1,Occurrences,R2,Temp1,RemainingElements).

occurrence_pairwise(_,[],_,_) :- !.
occurrence_pairwise(Element,[H|List],Occurrences,R2) :-
	sort([Element,H],Sorted),
	nth1(_,Occurrences,occurrence(Sorted,Occurrence)),
	Occurrence < R2,
	occurrence_pairwise(Element,List,Occurrences,R2).

