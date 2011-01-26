:- module mergesort.

:- interface.

:- import_module list.

:- func mergesort(list(A)) = list(A) is det.

:- implementation.

:- type seq(A) ---> seq(comparison_result, list(A)).

:- import_module int.
:- import_module exception.

:- func ordering_mix(comparison_result, comparison_result) = comparison_result is semidet.
:- func break_seqs(list(A)) = list(seq(A)).
:- func break_seqs(comparison_result, A, list(A), list(A)) = list(seq(A)).
:- func merge_seqs(list(seq(A))) = seq(A).
:- pred merge_seqs(list(seq(A))::in, int::in, seq(A)::out, list(seq(A))::out) is det.
:- func merge_two_seqs(seq(A), seq(A)) = seq(A).
:- func merge_equal_lists(list(A), list(A)) = seq(A).
:- func merge_sorted_lists(comparison_result, list(A), list(A)) = list(A).

ordering_mix((>), (>)) = (>).
ordering_mix((>), (=)) = (>).
ordering_mix((=), (>)) = (>).
ordering_mix((<), (<)) = (<).
ordering_mix((<), (=)) = (<).
ordering_mix((=), (<)) = (<).
ordering_mix((=), (=)) = (=).

mergesort(L) = S :-
        break_seqs(L) = Seqs,
        merge_seqs(Seqs) = seq(O, S1),
        (   O = (>)
        ->  S = S1
        ;   S = list.reverse(S1) ).

% next_seq(O, A, [B|L], Seq, O1, Tail) :-
%         (   O1 = ordering_mix(O, ordering(A, B))
%         ->  next_seq(O1, B, L, Seq1, O1, Tail),
%             Seq = [B|Seq1]
%         ;   Tail = [B|L],
%             O1 = O,
%             Seq = [A] ).

% break_seqs([]) = [].
% break_seqs([A|L]) = Seqs :-
%         next_seq((=), A, L, Seq, O, Tail),
%         break_seqs(Tail) = Seqs1,
%         Seqs = [seq(O,Seq)|Seqs1].

break_seqs([H|T]) = break_seqs((=), H, T, []).
break_seqs([]) = [].

break_seqs(O, A, [], C) = [seq(O, [A|C])].
break_seqs(O, A, [B|Tail], C) = Seqs :-
        (   O1 = ordering_mix(O, ordering(A, B))
        ->  break_seqs(O1, B, Tail, [A|C]) = Seqs
        ;   break_seqs((=), B, Tail, []) = Seqs1,
            Seqs = [seq(O, [A|C])|Seqs1] ).

merge_seqs([]) = seq((=), []).
merge_seqs([H|T]) = Sorted :-
        merge_seqs([H|T], length([H|T]), Sorted, _).

merge_seqs(L, Len, Sorted, Tail) :-
        Half = Len / 2,
        (   Half > 0
        ->  merge_seqs(L, Half, Sorted1, Tail1),
            merge_seqs(Tail1, Len - Half, Sorted2, Tail),
            merge_two_seqs(Sorted1, Sorted2) = Sorted
        ;   (   L = [_|_]
            ->  L = [Sorted|Tail]
            ;   throw("bad L argument"))).

merge_two_seqs(seq(O1, L1), seq(O2, L2)) = seq(O, L) :-
        (   O3 = ordering_mix(O1, O2)
        ->  (   O3 = (=)
            ->  merge_equal_lists(L1, L2) = seq(O, L)
            ;   merge_sorted_lists(O3, L1, L2) = L,
                O3 = O)
        ;   merge_sorted_lists(O1, L1, list.reverse(L2)) = L,
            O = O1 ).

merge_equal_lists([], []) = seq((=), []).
merge_equal_lists([], [H|T]) = seq((=), [H|T]).
merge_equal_lists([H|T], []) = seq((=), [H|T]).
merge_equal_lists([H1|T1], [H2|T2]) = seq(ordering(H1, H2), [H1|T1] ++ [H2|T2]).

merge_sorted_lists(_, [], []) = [].
merge_sorted_lists(_, [], [H|T]) = [H|T].
merge_sorted_lists(_, [H|T], []) = [H|T].
merge_sorted_lists(O, [H1|T1], [H2|T2]) = L :-
        (   compare(O, H2, H1)
        ->  L = [H1 | merge_sorted_lists(O, T1, [H2|T2])]
        ;   L = [H2 | merge_sorted_lists(O, [H1|T1], T2)]).
