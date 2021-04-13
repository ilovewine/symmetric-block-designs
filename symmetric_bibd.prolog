:- module(symmetric_bibd,[symmetric_bibd/3]).
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
	choose_blocks(N,K,R2,Occurrences,Pairs,[],BD).
choose_blocks(N,_,R2,Occurrences,_,BD,BD) :- length(BD,N), check_occurrences(Occurrences,R2), !.
choose_blocks(N,K,R2,Occurrences,[Pair|Pairs],Temp,BD) :-
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence < R2,
	Remainder is K - 2,
	create_block(N,Pair,Remainder,Occurrences,R2,Temp,Block,NewOccurrences),
	sort([Block|Temp],Temp1),
	choose_blocks(N,K,R2,NewOccurrences,[Pair|Pairs],Temp1,BD).
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

create_block(N,Pair,Remainder,Occurrences,R2,BD,Block,NewOccurrences) :-
	create_block(N,Pair,Remainder,Occurrences,R2,BD,Pair,Block,NewOccurrences).
create_block(_,_,0,Occurrences,R2,BD,Block,NewBlock,NewOccurrences) :-
	\+ member(Block,BD),
	sort(Block,NewBlock),
    length(Block,K),
    length(NewBlock,K),
	intersection_test(Block,BD,R2),
	map_occurrences(Occurrences,R2,Block,NewOccurrences).
create_block(N,Pair,Remainder,Occurrences,R2,BD,Temp,Block,NewOccurrences) :-
	find_element(R2,Occurrences,Temp,NewTemp),
	intersection_test(NewTemp,BD,R2,midtest),
	Remainder1 is Remainder-1,
	create_block(N,Pair,Remainder1,Occurrences,R2,BD,NewTemp,Block,NewOccurrences).

find_element(R2,Occurrences,Block,NewBlock) :-
	member(El,Block),
	get_occurrence([El,El2],Occurrences,Occurrence),
	\+ member(El2,Block),
	Occurrence < R2,
	occurrence_test([El2|Block],Occurrences,R2),
	NewBlock = [El2|Block].

intersection_test(_,[],_).
intersection_test(Block,[H|Blocks],R2) :-
	intersection(Block,H,Intersection),
	length(Intersection,R2),
	intersection_test(Block,Blocks,R2).
	
intersection_test(_,[],_,midtest).
intersection_test(Block,[H|Blocks],R2,midtest) :-
	intersection(Block,H,Intersection),
	length(Intersection,X),
	X =< R2,
	intersection_test(Block,Blocks,R2,midtest).

occurrence_test(Block,Occurrences,R2) :-
	block_pairs(Block,Pairs),
	occurrence_pair_test(Pairs,Occurrences,R2).

occurrence_pair_test([],_,_).
occurrence_pair_test([Pair|Pairs],Occurrences,R2) :-
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence < R2,
	occurrence_pair_test(Pairs,Occurrences,R2).