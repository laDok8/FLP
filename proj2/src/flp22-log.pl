/*
FLP Project 2: Turing machine
Author: Ladislav Dokoupil
Login: xdokou14
Year: 2023
*/


read_line(L,C) :-
    get_char(C),
    (isEOL(C), L = [], !;
    read_line(LL,_), L = [C|LL] ).

isEOL(C) :-
    C == end_of_file;
    (char_code(C,Code), Code==10).

read_lines(Ls) :-
    read_line(L, C),
    (C == end_of_file, Ls=[] ;
    read_lines(LLs), Ls = [L|LLs]).
% above taken from labs

process_lines([], _) :- fail.
process_lines([L], L).
process_lines([LL|LLs],Ret) :- process_line(LL), process_lines(LLs,Ret).


process_line([S1, ' ', A, ' ', S2, ' ', B]) :-
    %not(state(S1, A, S2, B)),
    assert(state(S1, A, S2, B)).

print_list([]):-nl. %nl = newline
print_list([H|T]):-write(H),print_list(T).

main :-
        % read stdin
        prompt(_, ''),
        read_lines(In),
        % process input -> assert States using dynamic predicate and returns Input State
        dynamic(state/4),
        process_lines(In, InpWord),
        % prepend S symbol as Start State
        print_list(["S"|InpWord]).
        
