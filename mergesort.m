:- module mergesort.

:- interface.

:- import_module list.
:- func mergesort(list(A)) = list(A) is det.

:- implementation.

:- import_module int.

:- func break_unsorted(comparison_result, A, list(A), list(A), list(list(A))) = list(list(A)).

:- func ordermix(comparison_result, comparison_result) = comparison_result is semidet.

ordermix((>), (>)) = (>).
ordermix((>), (=)) = (>).
ordermix((=), (>)) = (>).
ordermix((<), (<)) = (<).
ordermix((<), (=)) = (<).
ordermix((=), (<)) = (<).
        
break_unsorted(O, A, T, Current, Acu) = P :-
        (   T = [B|T1]
        ->  (   O1 = ordermix(O, ordering(A, B))
            ->  P = break_unsorted(O1, B, T1, [A|Current], Acu)
            ;   (   O = (<)
                ->  Current1 = list.reverse([A|Current])
                ;   Current1 = [A|Current]),
                P = break_unsorted((=), B, T1, [], [Current1|Acu]))
        ;   (   O = (<)
            ->  Current1 = list.reverse([A|Current])
            ;   Current1 = [A|Current] ),
            P = [Current1|Acu] ).

:- func mymerge(list(A), list(A)) = list(A) is det.

mymerge(L1, L2) = Merged :-
        (   L1 = [H1|T1]
        ->  (   L2 = [H2|T2]
            ->  (   H2 @> H1
                ->  T = mymerge(T1, L2),
                    Merged = [H1|T]
                ;   T = mymerge(L1, T2),
                    Merged = [H2|T])
            ;   Merged = L1 )
        ; Merged = L2).

:- pred mymerge(list(list(A))::in, int::in, list(A)::out, list(list(A))::out) is det.

mymerge(List, Len, Merged, Tail) :-
        Half = Len / 2,
        (   Half = 0
        ->  (   List = [_|_]
            ->  List = [Merged|Tail]
            ;   Merged = [],
                Tail = [])            
        ;   mymerge(List, Half, Merged1, Tail1),
            mymerge(Tail1, Len - Half, Merged2, Tail),
            Merged = mymerge(Merged1, Merged2)).


:- func mymerge(list(list(A))) = list(A) is det.

mymerge(L) = Merged:-
        Len = length(L),
        mymerge(L, Len, Merged, _).



mergesort(L) = Sorted :-
        (   L = [A|L1]
        ->  P = break_unsorted((=), A, L1, [], []),
            Sorted = mymerge(P)
        ;   Sorted = L).

            