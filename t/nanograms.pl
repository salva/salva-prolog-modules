% -*- Mode: Prolog -*-

:- use_module(transpose).

matrix(Mat, A) :-
        matrix(Mat, A, A).

matrix(Mat, A, B) :-
        length(Mat, A),
        matrix1(Mat, B).

matrix1([], _).
matrix1([H|T], B) :-
        length(H, B),
        matrix1(T, B).

write_matrix([]) :- nl.
write_matrix([H|T]) :-
        write_row(H),
        write_matrix(T).

write_row(R) :- write(R), nl.
        

nanogram(H, V, M) :-
        length(H, LH),
        length(V, LV),
        matrix(M, LH, LV),
        transpose(M, MT),
        nanogram(H, V, M, MT).

nanogram([], V, [], MT) :-
        (   V = []
        ->  MT = []
        ;   nanogram(V, [], MT, [])).

nanogram([W|T], V, [R|M], MT) :-
        put_row(R, W),
        nanogram(V, T, MT, M).

put_row([], []).
put_row([H|T], W) :-
        put_row(H, T, W).

put_row('x', T, [W|M]) :-
        succ(W1, W),
        (   W1 > 0
        ->  T = ['x'|_],
            put_row(T, [W1|M])
        ;   put_row_space(T, M)).
put_row('.', T, W) :-
        put_row(T, W).

put_row_space([], []).
put_row_space(['.'|T], W) :-
        put_row(T, W).

