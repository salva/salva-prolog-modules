% -*- Mode: Prolog -*-

:- module(randlist, [randlist/2,
                     randlist/4]).

:- use_module(library(random)).

randlist(Len, Min, Max, Out) :-
        (   Len = 0
        ->  Out = []
        ;   succ(Len1, Len),
            random(Min, Max, R),
            Out = [R|T],
            randlist(Len1, Min, Max, T)).

randlist(Len, Out) :-
        (   Len = 0
        ->  Out = []
        ;   succ(Len1, Len),
            random(R),
            Out = [R|T],
            randlist(Len1, T)).
