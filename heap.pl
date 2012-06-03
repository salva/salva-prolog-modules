% -*- Mode: Prolog -*-

:- module(heap, [ heap_empty/1,
                  heap_push/4,
                  heap_peek/3,
                  heap_peek/2,
                  heap_pop/4,
                  heap_pop/3,
                  heap_merge/3 ]).

heap_empty(h).

heap_push(H, E, P, O) :-
        heap_merge(H, h(E, P, h, h, 1), O).

heap_merge(H1, H2, O) :-
        (   H1 = h(E1, P1, L1, R1, _)
        ->  (   H2 = h(E2, P2, L2, R2, _)
            -> (   P1 =< P2
               ->  (   heap_merge(L1, H2, L11),
                       L11 = h(_, _, _, _, DL11),
                       (   R1 = h(_, _, _, _, DR1)
                       ->  (   DR1 < DL11
                           ->  DO is DL11 + 1,
                               O = h(E1, P1, L11, R1, DO)
                           ;   DO is DR1 + 1,
                               O = h(E1, P1, R1, L11, DO) )
                       ;   O = h(E1, P1, h, L11, 1) ) )
               ;   (   heap_merge(L2, H1, L21),
                       L21 = h(_, _, _, _, DL21),
                       (   R2 = h(_, _, _, _, DR2)
                       ->  (   DR2 < DL21
                           ->  DO is DL21 + 1,
                               O = h(E2, P2, L21, R2, DO)
                           ;   DO is DR2 + 1,
                               O = h(E2, P2, R2, L21, DO) )
                       ;   O = h(E2, P2, h, L21, 1) ) ) )
            ;   O = H1 )
        ;   O = H2 ).

heap_peek(h(E, P, _, _, _), E, P).
heap_peek(h(E, _, _, _, _), E).

heap_pop(h(E, P, L, R, _), E, P, O) :-
        heap_merge(L, R, O).
heap_pop(h(E, _, L, R, _), E, O) :-
        heap_merge(L, R, O).