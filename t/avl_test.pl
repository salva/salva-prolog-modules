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

:- times_merge([1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16000, 32000, 64000, 128000]).
