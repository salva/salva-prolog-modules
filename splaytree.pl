% -*- Mode: Prolog -*-

:- module(splaytree,
          [spt_new/1,
           spt_put/4,
           spt_get/4,
           list_to_spt/2,
           spt_maplist/4,
           spt_dump/1,

           sptw_new/1,
           sptw_put/3,
           sptw_get/3,
           list_to_sptw/2,
           sptw_maplist/3,
           sptw_dump/1
           ]).

spt_new(spt).

spt_put(T, K, V, spt(K, V, L, R)) :-
        put(T, K, L, R).

put(spt, _, spt, spt).

put(spt(KO, VO, LO, RO), K, L, R) :-
        compare(O, K, KO),
        put(O, KO, VO, LO, RO, K, L, R).

put(<, KO, VO, LO, RO, K, L1, spt(KO, VO, R1, RO)) :-
        put(LO, K, L1, R1).

put(>, KO, VO, LO, RO, K, spt(KO, VO, LO, L1), R1) :-
        put(RO, K, L1, R1).

spt_get(spt(KO, VO, LO, RO), K, V, spt(K, V, L, R)) :-
        get(KO, VO, LO, RO, K, V, L, R).

get(KO, VO, LO, RO, K, V, L, R) :-
        compare(O, K, KO),
        get(O, KO, VO, LO, RO, K, V, L, R).

get(=, K, V, L, R, K, V, L, R).

get(<, KO, VO, spt(KLO, VLO, LLO, RLO), RO, K, V, L1, spt(KO, VO, R1, RO)) :-
        get(KLO, VLO, LLO, RLO, K, V, L1, R1).

get(>, KO, VO, LO, spt(KRO, VRO, LRO, RRO), K, V, spt(KO, VO, LO, L1), R1) :-
        get(KRO, VRO, LRO, RRO, K, V, L1, R1).


list_to_spt(L, T) :-
        spt_new(T1),
        append_list(L, T1, T).

append_list([], T, T).
append_list([K-V|L], T, T1) :-
        spt_put(T, K, V, T2),
        append_list(L, T2, T1).

spt_maplist(T, L, L1, T1) :-
        maplist(L, T, L1, T1).

maplist([], T, [], T).
maplist([K|L], T, [K-V|L1], T1) :-
       spt_get(T, K, V, T2),
       maplist(L, T2, L1, T1).

sptw_new(sptw(T)) :-
        spt_new(T).

sptw_put(W, K, V) :-
        W = sptw(T),
        spt_put(T, K, V, T1),
        setarg(1, W, T1).

sptw_get(W, K, V) :-
        W = sptw(T),
        spt_get(T, K, V, T1),
        setarg(1, W, T1).

list_to_sptw(L, sptw(T)) :-
        list_to_spt(L, T).

sptw_maplist(W, L, M) :-
        W = sptw(T),
        spt_maplist(T, L, M, T1),
        setarg(1, W, T1).

spt_dump(T) :-
	spt_dump(T, '').
spt_dump(spt, S) :-
	format('~pspt~n', [S]).
spt_dump(spt(K, V, L, R), S) :-
	format('~pspt ~p=~p~n', [S, K, V]),
	atom_concat(S, '   |', SL),
	spt_dump(L, SL),
	atom_concat(S, '    ', SR),
	spt_dump(R, SR).

sptw_dump(sptw(T)) :-
        spt_dump(T).