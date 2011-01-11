:- module splaytree.

:- interface.

:- import_module list, pair.

:- type splaytree(K, V) ---> [] ; splayt(K, V, splaytree(K, V), splaytree(K, V)).

:- func new = splaytree(_, _).

:- func new(list(pair(K, V))) = splaytree(K, V) is semidet.

:- pred put(splaytree(K, V), K, V, splaytree(K, V)).
:- mode put(in, in, in, out) is semidet.

:- pred get(splaytree(K, V), K, V, splaytree(K, V)).
:- mode get(in, in, out, out) is semidet.

:- pred put_list(splaytree(K, V), list(pair(K, V)), splaytree(K, V)).
:- mode put_list(in, in, out) is semidet.

:- implementation.

new = [].

new(L) = T :-
        T1 = new,
        put_list(T1, L, T).

:- pred put1(comparison_result, K, V, splaytree(K, V), splaytree(K, V), K, splaytree(K, V), splaytree(K, V)).
:- mode put1(in, in, in, in, in, in, out, out) is semidet.

put1((<), KO, VO, LO, RO, K, L1, R) :-
        put1(LO, K, L1, R1),
        R = splayt(KO, VO, R1, RO).

put1((>), KO, VO, LO, RO, K, L, R1) :-
        put1(RO, K, L1, R1),
        L = splayt(KO, VO, LO, L1).

:- pred put1(splaytree(K, V), K, splaytree(K, V), splaytree(K, V)).
:- mode put1(in, in, out, out) is semidet.

put1([], _, [], []).
put1(splayt(KO, VO, LO, RO), K, L, R) :-
        compare(O, K, KO),
        put1(O, KO, VO, LO, RO, K, L, R).

put(T, K, V, Out) :-
        put1(T, K, L, R),
        Out = splayt(K, V, L, R).

put_list(T, [], T).
put_list(T, [K-V|L], T1) :-
        put(T, K, V, T2),
        put_list(T2, L, T1).


:- pred get1(comparison_result, K, V, splaytree(K, V), splaytree(K, V), K, V, splaytree(K, V), splaytree(K, V)).
:- mode get1(in, in, in, in, in, in, out, out, out).

get1((=), K, V, L, R, K, V, L, R).
get1((<), KO, VO, splayt(KLO, VLO, LLO, RLO), RO, K, V, L1, R) :-
        get1(KLO, VLO, LLO, RLO, K, V, L1, R1),
        R = splayt(KO, VO, R1, RO).
get1((>), KO, VO, LO, splayt(KRO, VRO, LRO, RRO), K, V, L, R1) :-
        get1(KRO, VRO, LRO, RRO, K, V, L1, R1),
        L = splayt(KO, VO, LO, L1).

:- pred get1(K, V, splaytree(K, V), splaytree(K, V), K, V, splaytree(K, V), splaytree(K, V)).
:- mode get1(in, in, in, in, in, out, out, out).

get1(KO, VO, LO, RO, K, V, L, R) :-
        compare(O, K, KO),
        get1(O, KO, VO, LO, RO, K, V, L, R).

get(splayt(KO, VO, LO, RO), K, V, Out) :-
        get1(KO, VO, LO, RO, K, V, L, R),
        Out = splayt(K, V, L, R).

