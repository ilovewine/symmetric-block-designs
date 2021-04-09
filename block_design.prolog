:- use_module(library(clpfd)).

bibd(N,K,R2) :-
	bibd(N,K,R2,BD), !,
	setof(X,between(1,N,X),Set),
	write_list(Set),
	writeln_list(BD).
bibd(_,_,_) :- writeln('Such 2-BIBD does not exist.').

write_list([El|[]]) :- writeln(El).
write_list([EL|List]) :- write(El), tab(1), write_list(List).

writeln_list([]).
writeln_list([El|List]) :- writeln(El), writeln_list(List).

bibd(N,K,R2,BD) :-
    R1 is R2*(N-1)/(K-1),
    R1 =:= K,
    subsets(N,K,SubsetFamily),
    writeln(SubsetFamily),
    choose_subsets(N,R2,SubsetFamily,BD), !.

set_of_size(N,K,Subset) :-
    length(Block,K),
    Block ins 1..N,
    all_distinct(Block),
    label(Block),
    sort(Block,Subset).

subsets(N,K,Family) :- setof(Subset,set_of_size(N,K,Subset),Family).

choose_subsets(N,R2,SubsetFamily,BD) :-
	different_blocks(N,SubsetFamily,BD),
	test_r2(BD,BD,R2).

different_blocks(N,Family,BD) :-
    length(Indexes,N),
    length(Family,FamilyLength),
    Indexes ins 1..FamilyLength,
    all_distinct(Indexes),
    map_bd(Indexes,Family,BD).

map_bd(Indexes,Family,BD) :- map_bd(Indexes,Family,[],BD).
map_bd([],_,BD,BD) :- !.
map_bd([Index|Indexes],Family,Temp,BD) :-
	nth1(Index,Family,Subset),
	Temp1 = [Subset|Temp],
	map_bd(Indexes,Family,Temp1,BD).

test_r2([],_,_) :- !.
test_r2([Block|Blocks],BD,R2) :-
    subtract(BD,[Block],BD1),
    check_intersection(Block,BD1,R2),
    test_r2(Blocks,BD,R2).

check_intersection(_,[],_) :- !.
check_intersection(Block1,[Block2|BD],R2) :-
    intersection(Block1,Block2,Intersection),
    length(Intersection,R2),
    check_intersection(Block1,BD,R2).

% [[1,2,3],[3,4,5],[5,6,1],[2,4,6],[1,7,4],[3,7,6],[2,7,5]]