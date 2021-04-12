:- module(write_list,[write_list/1,writeln_list/1]).

write_list([El|[]]) :- writeln(El).
write_list([El|List]) :- write(El), tab(1), write_list(List).

writeln_list([]).
writeln_list([El|List]) :- write_list(El), writeln_list(List).