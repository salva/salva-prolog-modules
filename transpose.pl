% -*- Mode: Prolog -*-

:- module(transpose,
          [transpose/2]).

transpose(M, T) :-
        pop_column(M, M1, C),
        (   C = []
        ->  T = []
        ;   T = [C|T1],
            transpose(M1, T1)).

pop_column([], [], []).
pop_column([H|T], M, C) :-
        (   H = [E|F]
        ->  M = [F|M1],
            C = [E|C1],
            pop_column(T, M1, C1)
        ;   pop_column(T, M, C)).
