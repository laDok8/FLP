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
% ^^^ taken from labs

process_lines_core([], _) :- fail.
process_lines_core([L], L) :- !.
process_lines_core([LL|LLs],Ret) :- process_line(LL), process_lines_core(LLs,Ret).
% separation to fail if no states were defined
process_lines([L|LLs], Ret) :- process_line(L), process_lines_core(LLs,Ret).


:- dynamic state/4.
process_line([S1, ' ', A, ' ', S2, ' ', B]) :-
    state(S1, A, S2, B) -> true;assert(state(S1, A, S2, B)).



% print list with Head
print_word(Word, _, _, CurP) :- length(Word, Len), CurP >= Len, nl,!.
print_word(Word, State, HeadPos, CurP) :-
    CurP == HeadPos -> 
        write(State), nth0(CurP, Word, Symb), write(Symb), Nex is CurP + 1, print_word(Word, State, HeadPos, Nex);
        (nth0(CurP, Word, Symb), write(Symb), Nex is CurP + 1, print_word(Word, State, HeadPos, Nex)).



replace_nth0(Index, List, New, Result) :-
    nth0(Index,List,Old),
    nth0(Index, List, Old, Rest),
    nth0(Index, Result, New, Rest).

% generate single step (State, Pos, Word) -> (NState, NPos, NWord)
:- dynamic was_here/3.
make_step(State, Pos, Word, NState, NPos, NWord) :-
    assert(was_here(State, Pos, Word)),
    nth0(Pos, Word, CurSymb),
    state(State, CurSymb, NState, NSymb),
    (NSymb == 'R' ->
        NPos is Pos + 1,NWord = Word;
        (NSymb == 'L' -> 
            NPos is Pos - 1; 
            (NPos is Pos,replace_nth0(Pos, Word, NSymb, NWord)))),
    not(was_here(NState, NPos, NWord)).

% path as tuple
make_steps('F', Pos, Word, Path) :- Path = (Word,'F',Pos),!.
make_steps(State, Pos, Word, [(Word, State, Pos)|Path]) :-
    make_step(State, Pos, Word, NState, NPos, NWord),
    make_steps(NState, NPos, NWord, Path).




% path: list of tuples: [(Word, State, Pos)]
print_path([]) :- nl,!.
print_path((Word, State, Pos)) :- print_word(Word, State, Pos, 0),!.
print_path([(Word, State, Pos)|Path]) :-
    print_word(Word, State, Pos, 0),
    print_path(Path).

main :-
        % read stdin
        prompt(_, ''),
        read_lines(In),
        % process input -> assert States using dynamic predicate and returns Input State
        process_lines(In, InpWord),!,
        make_steps('S', 0, InpWord, Path),
        print_path(Path),!.
