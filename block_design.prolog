:- use_module(library(clpfd)).

bibd(N,K,R2,BD) :-
    subsets(N,K,SubsetFamily), !,
    choose_subsets2(N,R2,SubsetFamily,BD), !.

set_of_size(N,K,Subset) :-
    length(Block,K),
    Block ins 1..N,
    all_distinct(Block),
    label(Block),
    sort(Block,Subset).

subsets(N,K,Family) :- setof(Subset,set_of_size(N,K,Subset),Family).

choose_subsets(N,R2,SubsetFamily,BD) :-
    length(BDIndexes,N),
    length(SubsetFamily,SubsetLength),
    BDIndexes ins 1..SubsetLength,
    all_distinct(BDIndexes),
    label(BDIndexes),
    map_bd(BDIndexes,SubsetFamily,BD).
    %number_of_occurences(N,BD,R2).

map_bd(BDIndexes,SubsetFamily,BD) :- map_bd(BDIndexes,SubsetFamily,[],BD).
map_bd([],_,BD,BD) :- !.
map_bd([Index|BDIndexes],SubsetFamily,Temp,BD) :-
    nth1(Index,SubsetFamily,Elem),
    Temp1 = [Elem|Temp],
    map_bd(BDIndexes,SubsetFamily,Temp1,BD).

number_of_occurences(N,BD,R2) :-
    produce_pair(N,Pair),
    \+ pair_occurence(Pair,BD,R2), !,
    fail.
number_of_occurences(N,BD,R2) :-
    produce_pair(N,Pair),
    pair_occurence(Pair,BD,R2).

produce_pair(N,Pair) :-
    length(Pair,2),
    Pair ins 1..N,
    all_distinct(Pair),
    label(Pair).

pair_occurence(Pair,BD,R2) :- pair_occurence(Pair,BD,0,R2).
pair_occurence(_Pair,[],R2,R2) :- !.
pair_occurence(Pair,[Block|BD],Index,R2) :- subset(Pair,Block), !, Index1 is Index+1, pair_occurence(Pair,BD,Index1,R2).
pair_occurence(Pair,[Block|BD],Index,R2) :- \+ subset(Pair,Block), !, pair_occurence(Pair,BD,Index,R2).

choose_subsets2(N,R2,SubsetFamily,BD) :-
	subsets(N,2,Pairs),
	choose_subsets_from_pairs(Pairs,Pairs,SubsetFamily,R2,[],BD).

choose_subsets_from_pairs(_Pairs,[],_Family,_R2,BD,BD) :- !.
choose_subsets_from_pairs(PairFamily,[Pair|Pairs],Family,R2,Temp,BD) :-
	count_existing_pair(Pair,Temp,K),
	K = R2,
	choose_subsets_from_pairs(PairFamily,Pairs,Family,R2,Temp,BD), !.
choose_subsets_from_pairs(PairFamily,[Pair|_Pairs],Family,R2,Temp,BD) :-
	count_existing_pair(Pair,Temp,K),
	Remaining is R2-K,
	Remaining > 0, !,
	permutation(Family, PermFamily),
	find_remaining_subsets(Pair,PermFamily,Temp,Remaining,Subsets),
	union(Temp,Subsets,Temp1),
	choose_subsets_from_pairs(PairFamily,PairFamily,Family,R2,Temp1,BD), !.
choose_subsets_from_pairs(_PairFamily,_Pairs,_Family,_R2,_Temp,_BD) :- fail.

count_existing_pair(Pair,Subsets,K) :- count_existing_pair(Pair,Subsets,0,K).
count_existing_pair(_Pair,[],K,K) :- !.
count_existing_pair(Pair,[Subset|Subsets],Index,K) :-
	subset(Pair,Subset),
	Index1 is Index+1,
	count_existing_pair(Pair,Subsets,Index1,K), !.
count_existing_pair(Pair,[_|Subsets],Index,K) :- count_existing_pair(Pair,Subsets,Index,K), !.

find_remaining_subsets(Pair,SubsetFamily,Subsets,RemainingNumber,RemainingSubsets) :-
	find_remaining_subsets(Pair,SubsetFamily,Subsets,RemainingNumber,[],RemainingSubsets).
find_remaining_subsets(_Pair,_SubsetFamily,_Subsets,0,RemainingSubsets,RemainingSubsets) :- !.
find_remaining_subsets(Pair,[Subset|SubsetFamily],Subsets,RemainingNumber,Temp,RemainingSubsets) :-
	subset(Pair,Subset),
	\+ member(Subset,Subsets),
	Number is RemainingNumber-1,
	Temp1 = [Subset|Temp],
	find_remaining_subsets(Pair,SubsetFamily,Subsets,Number,Temp1,RemainingSubsets), !.
find_remaining_subsets(Pair,[_,SubsetFamily],Subsets,RemainingNumber,Temp,RemainingSubsets) :-
	find_remaining_subsets(Pair,SubsetFamily,Subsets,RemainingNumber,Temp,RemainingSubsets).

% [[4,1,5],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3],[1,2,3]]
% [[1,2,3],[3,4,5],[5,6,1],[2,4,6],[1,7,4],[3,7,6],[2,7,5]]