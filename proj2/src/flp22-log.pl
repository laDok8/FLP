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

process_lines([], _) :- fail.
process_lines([L], L).
process_lines([LL|LLs],Ret) :- process_line(LL), process_lines(LLs,Ret).


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

% todo check end
% generate 1 step State, Pos, Word
makeStep(State, Pos, Word, NState, NPos, NWord) :-
    nth0(Pos, Word, CurSymb),
    state(State, CurSymb, NState, NSymb),
    (NSymb == 'R' -> 
        NPos is Pos + 1,NWord = Word;
        (NSymb == 'L' -> 
            NPos is Pos - 1; 
            (NPos is Pos,replace_nth0(Pos, Word, NSymb, NWord),!
    ))).
%usage: makeStep('S', 0, [a,b,c], NState, NPos, NWord).

main :-
        % read stdin
        prompt(_, ''),
        read_lines(In),
        % process input -> assert States using dynamic predicate and returns Input State
        dynamic(state/4),
        process_lines(In, InpWord),
        print_word(InpWord, 'S', 0, 0),
        makeStep('S', 0, InpWord, NState, NPos, NWord),
        print_word(NWord, NState, NPos, 0),!.
        
