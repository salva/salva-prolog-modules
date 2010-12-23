% -*- Mode: Prolog -*-

:- module(unique, [ unique/2,
                    keymerge/2]).

:- use_module(hashtable).

unique(L, U) :-
        ht_new(HT),
        unique(L, HT, U).

unique([], _, []).

unique([H|T], HT, U) :-
        (   ht_put(HT, H)
        ->  U = [H|U1],
            unique(T, HT, U1)
        ;   unique(T, HT, U)).

keymerge(L, U) :-
        ht_new(HT),
        ht_append_list(HT, L),
        ht_to_list(HT, U).
