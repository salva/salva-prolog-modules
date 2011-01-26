:- module test_mergesort.

:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module mergesort.
:- import_module list.

main(!IO) :-
        O = [1, 2, 3, 1, 2, 3, 2, 1, 0, 0, -1, 4, 5, 5, 5, 6, -1, -2, -4, -3, -1, -5, -12],
        S = mergesort.mergesort(O),
        Seqs = mergesort.break_seqs(O),
        list.sort(O, T),
        io.write(O, !IO),
        io.nl(!IO),
        io.write(Seqs, !IO),
        io.nl(!IO),
        io.write(S, !IO),
        io.nl(!IO),
        io.write(T, !IO),
        io.nl(!IO).