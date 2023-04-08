/*
FLP Project 2: Turing machine
Author: Ladislav Dokoupil
Login: xdokou14
Year: 2023
*/

:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).


main :-
        read_line_to_codes(user_input, Input),
        write(Input),
        nl,
        halt.
