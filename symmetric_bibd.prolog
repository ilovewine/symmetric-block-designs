:- module(symmetric_bibd,[symmetric_bibd/3]).
:- use_module(library(clpfd)).
:- use_module(set_subset).
:- use_module(write_list).

symmetric_bibd(N,K,R2) :-
	check_necessary_conditions(N,K,R2),
	symmetric_bibd(N,K,R2,BD), % MAIN PROGRAM
	% WRITE RESULTS, END PROGRAM WITH SUCCESS
	set_of_length(N,Set),
	write_list(Set),
	writeln_list(BD), !.
symmetric_bibd(_,_,_) :- writeln('Such symmetric 2-bibd does not exist.').

symmetric_bibd(N,K,R2,BD) :-
	subsets(N,2,Pairs),
	% CREATE ARTIFICAL occurrence(Pair, Number) functors in order to count occurrences to test them in further steps
	create_occurrences(Pairs,Occurrences),
	choose_blocks(N,K,R2,Occurrences,Pairs,[],BD).

% FINISH, TEST RESULTS
choose_blocks(N,_,R2,Occurrences,_,BD,BD) :- length(BD,N), final_occurrence_test(Occurrences,R2), !.
% MAIN RECURSIVE PROGRAM
choose_blocks(N,K,R2,Occurrences,[Pair|Pairs],Temp,BD) :-
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence < R2,
	Remainder is K - 2,
	% CREATE A COMPATIBLE BLOCK
	create_block(N,Pair,Remainder,Occurrences,R2,Temp,Block,NewOccurrences),
	sort([Block|Temp],Temp1),
	choose_blocks(N,K,R2,NewOccurrences,[Pair|Pairs],Temp1,BD).
choose_blocks(N,K,R2,Occurrences,[Pair|Pairs],Temp,BD) :-
	get_occurrence(Pair,Occurrences,Occurrence),
	Occurrence = R2,
	choose_blocks(N,K,R2,Occurrences,Pairs,Temp,BD).

create_occurrences(Pairs,Occurrences) :- create_occurrences(Pairs,[],Occurrences).
create_occurrences([],Occurrences,Occurrences) :- !.
create_occurrences([Pair|Pairs],Temp,Occurrences) :-
	% HERE WE CREATE an occurrence functor for the specific Pair
	Occurrence = occurrence(Pair,0),
	Temp1 = [Occurrence|Temp],
	create_occurrences(Pairs,Temp1,Occurrences).

final_occurrence_test([],_) :- !.
final_occurrence_test([Occurrence|Occurrences],R2) :-
	arg(2,Occurrence,R2),
	final_occurrence_test(Occurrences,R2).

get_occurrence(Pair,Occurrences,Result) :- nth1(_,Occurrences,occurrence(Pair,Result)).

change_occurrences(Occurrences,R2,Block,Result) :- block_pairs(Block,Pairs), change_pairs_occurrences(Pairs,R2,Occurrences,Result).
change_pairs_occurrences([],_,Result,Result) :- !.
change_pairs_occurrences([Pair|Pairs],R2,Occurrences,Result) :-
	nth1(_,Occurrences,occurrence(Pair,Occurrence)),
	Occurrence1 is Occurrence+1,
	Occurrence1 =< R2,
	select(occurrence(Pair,Occurrence),Occurrences,occurrence(Pair,Occurrence1),Occurrences1),
	change_pairs_occurrences(Pairs,R2,Occurrences1,Result).

create_block(N,Pair,Remainder,Occurrences,R2,BD,Block,NewOccurrences) :-
	create_block(N,Pair,Remainder,Occurrences,R2,BD,Pair,Block,NewOccurrences).
create_block(_,_,0,Occurrences,R2,BD,Block,NewBlock,NewOccurrences) :-
	\+ member(Block,BD),
	sort(Block,NewBlock),
    length(Block,K),
    length(NewBlock,K),
	intersection_test(Block,BD,R2),
	change_occurrences(Occurrences,R2,Block,NewOccurrences).
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

% OPTIMISATION TESTS

check_necessary_conditions(N,K,R2) :-
	R1 is R2*(N-1)/(K-1),
	R1 =:= K.

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