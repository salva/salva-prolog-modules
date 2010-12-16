% -*- Mode: Prolog -*-

:- use_module(library(random)).
:- use_module(avl).

random_list(N, L) :-
        (   N = 0
        ->  L = []
        ;   random(X),
            L = [X-t|L1],
            N1 is N - 1,
            random_list(N1, L1)).

random_avl(N, T) :-
        random_list(N, L),
        list_to_avl(L, T).

range_avl(N, T) :-
        avl_empty(T1),
        range_avl(N, T1, T).

range_avl(0, T, T1) :- !, T = T1.
range_avl(N, T, T1) :-
        avl_put(T, N, T2),
        N1 is N - 1,
        range_avl(N1, T2, T1).

time_merge(N) :-
        random_list(N, L1),
        random_list(N, L2),
        list_to_avl(L1, T1),
        list_to_avl(L2, T2),
        format('timing avl_merge, size: ~w~n', [N]),
        time(avl_merge(T1, T2, _)).

times_merge([]).
times_merge([N|L]) :-
        time_merge(N),
        times_merge(L).

avl_delete_keys(T, L) :-
        (   avl_delete_first(T, T1, K, _)
        ->  avl_dump(T1),
            L = [K|L1],
            avl_delete_keys(T1, L1)
        ;   L = []).

/* :- times_merge([1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16000, 32000, 64000, 128000, 256000]). */
