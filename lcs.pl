% -*- Mode: Prolog -*- 

:- module(lcs, [lcs/3, lcs/4]).

:- use_module(hashtable).

lcs(A, B, O) :- lcs(A, B, O, _).

lcs_simple(A, B, O, Len) :-
        (   A = [HA|TA],
            B = [HB|TB]
        ->  (   HA = HB
            ->  O = [HA|TO],
                lcs(TA, TB, TO, Len1),
                Len is Len1 + 1
            ;   lcs(A, TB, O1, Len1),
                lcs(B, TA, O2, Len2),
                (   Len1 >= Len2
                ->  O = O1,
                    Len = Len1
                ;   O = O2,
                    Len = Len2 ) )
        ;   O = [],
            Len = 0 ).

lcs(A, B, O, Len) :-
        ht_new(Cache),
        lcs(A, B, O, Len, Cache).
        %ht_to_list(Cache, L),
        %write(L), nl.

lcs(A, B, O, Len, Cache) :-
        (   ht_get(Cache, A/B, O1/Len1)
        ->  O = O1,
            Len = Len1
        ;   (   A = [HA|TA],
                B = [HB|TB]
            ->  (   HA = HB
                ->  lcs(TA, TB, TO, Len1, Cache),
                    O = [HA | TO],
                    Len is Len1 + 1
                ;   lcs(A, TB, O1, Len1, Cache),
                    lcs(B, TA, O2, Len2, Cache),
                    (   Len1 >= Len2
                    ->  O = O1,
                        Len = Len1
                    ;   O = O2,
                        Len = Len2 ) )
            ;   O = [],
                Len = 0
            ),
            ht_put(Cache, A/B, O/Len) ).