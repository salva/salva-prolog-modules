% -*- Mode: Prolog -*-
/** <module> AVL tree library
  
  Copyright (C) 2004-2010 by Salvador Fandiño

  This library is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This library is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PrologDoc; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  @author Salvador Fandiño <sfandino@yahoo.com>
  @license GPL

*/

:- module(avl, [ avl_empty/1,
                 avl_to_list/2,
                 avl_keys/2,
                 avl_values/2,
                 avl_depth/2,
                 avl_put/4,
		 avl_put/3,
		 avl_replace/4,
		 avl_replace/3,
                 avl_replace_list/3,
		 avl_get/3,
		 avl_has/2,
                 avl_iterator/2,
                 avl_iterator_advance/3,
                 avl_iterator_advance/4,
                 avl_delete_first/2,
                 avl_delete_first/4,
                 avl_delete_last/2,
                 avl_delete_last/4,
                 avl_delete/3,
                 avl_delete/4,
                 avl_delete_list/3,
                 avl_delete_list/4,
		 ordered_list_to_avl/2,
		 list_to_avl/2,
                 keys_to_avl/2,
                 avl_merge/3,
                 avl_small_merge/3,
                 avl_gen/3,
                 avl_break/5,
                 avl_map/2,
                 avl_map/3,
                 avl_dump/1]).
  
%%  avl_empty(-Tree:avl) is det.
%
%  Creates an empty AVL tree.

avl_empty(t).


%% avl_to_list(+Tree:avl, -List:list_of_pairs) is det.
%
%  Converts an AVL tree to a sorted list.
%
%   @param Tree AVL tree
%   @param List ordered list with value-key pairs [K1-V1, K2-V2, ...].

avl_to_list(T, L) :-
	avl_to_list(T, L, []).

avl_to_list(t, L, L).
avl_to_list(avl(K, V, L, R, _), List, Rest) :-
	avl_to_list(L, List, [K-V|M]),
	avl_to_list(R, M, Rest).


%% avl_keys(+Tree:avl, -Keys:list) is det.
%
% Returns a list with the keys on the tree.

avl_keys(T, L) :-
        avl_keys(T, L, []).
avl_keys(t, L, L).
avl_keys(avl(K, _, L, R, _), List, Rest) :-
        avl_keys(L, List, [K|M]),
        avl_keys(R, M, Rest).


%% avl_values(+Tree:avl, -Values:avl) is det.
% Returns a list with the values on the tree.

avl_values(T, L) :-
        avl_values(T, L, []).
avl_values(t, L, L).
avl_values(avl(_, V, L, R, _), List, Rest) :-
        avl_values(L, List, [V|M]),
        avl_values(R, M, Rest).


%% avl_put(+Tree:avl, +Key, -Out:avl) is semidet.
% Equivalent to avl_put(Tree, Key, [], Out).

avl_put(T, K, T1) :-
        avl_put(T, K, [], T1).


%% avl_put(+Tree:avl, +Key:ground_term, +Value, -Out:avl) is semidet.
%
% Inserts the pair Key-Value into the tree.
%
% Fails when the tree already contains an element with the given key.

avl_put(T, K, V, T1) :-
        (   ground(K)
        ->  avl_put_unsafe(T, K, V, T1)
        ;   instantation_error(K)).

avl_put_unsafe(t, K, V, avl(K, V, t, t, 1)).

avl_put_unsafe(avl(NK, NV, L, R, D), K, V, T) :-
	compare(O, K, NK),
	avl_put(O, NK, NV, L, R, D, K, V, T).

avl_put(<, NK, NV, L, R, D, K, V, T) :-
	avl_put(L, K, V, L1),
	(   L1 = avl(_, _, _, _, D)
	->  avl_balance_left(NK, NV, L1, R, T)
	;   T = avl(NK, NV, L1, R, D) ).

avl_put(>, NK, NV, L, R, D, K, V, T) :-
	avl_put(R, K, V, R1),
	(   R1 = avl(_, _, _, _, D)
	->  avl_balance_right(NK, NV, L, R1, T)
	;   T = avl(NK, NV, L, R1, D) ).


%% avl_replace(+Tree:avl, +Key, -Out:avl) is det.
% Equivalent to avl_replace(Tree, Key, [], Out).

avl_replace(T, K, T1) :-
	avl_replace(T, K, [], T1).


%% avl_replace(+Tree:avl, +Key, +Value, -Out:avl) is det.
% Inserts the pair Key-Value into the tree.
%
% Replaces any previous entry with the given key.

avl_replace(T, K, V, T1) :-
        (   ground(K)
        ->  avl_replace_unsafe(T, K, V, T1)
        ;   instantation_error(K)).

avl_replace_unsafe(t, K, V, avl(K, V, t, t, 1)).
avl_replace_unsafe(avl(NK, NV, L, R, D), K, V, T) :-
	compare(O, K, NK),
	avl_replace(O, NK, NV, L, R, D, K, V, T).

avl_replace(=, NK, _, L, R, D, _, V, avl(NK, V, L, R, D)).
avl_replace(<, NK, NV, L, R, D, K, V, T) :-
	avl_replace(L, K, V, L1),
	(   L1 = avl(_, _, _, _, D)
	->  avl_balance_left(NK, NV, L1, R, T)
	;   T = avl(NK, NV, L1, R, D) ).

avl_replace(>, NK, NV, L, R, D, K, V, T) :-
	avl_replace(R, K, V, R1),
	(   R1 = avl(_, _, _, _, D)
	->  avl_balance_right(NK, NV, L, R1, T)
	;   T = avl(NK, NV, L, R1, D) ).


%% avl_replace_list(+Tree:avl, +List:list, -Out:avl) is det.
% Inserts the elements in the given list into the tree replacing
% previous entries if necessary.

avl_replace_list(T, L, T1) :-
        avl_replace_list1(L, T, T1).

avl_replace_list1([], T, T).
avl_replace_list1([K-V|L], T, T1) :-
        avl_replace(T, K, V, T2),
        avl_replace_list1(L, T2, T1).


%% avl_depth(+Tree:avl, -Depth:int) is det.
% Returns the tree depth.

avl_depth(t, 0).
avl_depth(avl(_,_,_,_,D), D).


avl_cmp_depth(t, D, D).
avl_cmp_depth(avl(_,_,_,_,AD), BD, D) :-
	D is BD - AD.

avl_balance_left(NK, NV, avl(LK, LV, LL, LR, LD), R, T) :-
	(   avl_cmp_depth(R, LD, 2)
	->  % avl_dump(avl(NK, NV, avl(LK, LV, LL, LR, LD),R, _), 'lb: '),nl,
	    (	LR = avl(LRK, LRV, LRL, LRR, LRD),
		avl_cmp_depth(LL, LRD, 1)
	    ->	T = avl(LRK, LRV, avl(LK, LV, LL, LRL, LRD), avl(NK, NV, LRR, R, LRD), LD)
	    ;	ND1 is LD-1,
		T = avl(LK, LV, LL, avl(NK, NV, LR, R, ND1), LD) )
	;   D1 is LD + 1,
	    T = avl(NK, NV, avl(LK, LV, LL, LR, LD), R, D1) ).
	
avl_balance_right(NK, NV, L, avl(RK, RV, RL, RR, RD), T) :-
	(   avl_cmp_depth(L, RD, 2)
	->  % avl_dump(avl(NK, NV, L, avl(RK, RV, RL, RR, RD), _), 'rb: '),nl,
	    (	RL = avl(RLK, RLV, RLL, RLR, RLD),
		avl_cmp_depth(RR, RLD, 1)
	    ->	T = avl(RLK, RLV, avl(NK, NV, L, RLL, RLD), avl(RK, RV, RLR, RR, RLD), RD)
	    ;	ND1 is RD-1,
		T = avl(RK, RV, avl(NK, NV, L, RL, ND1), RR, RD) )
	;   D1 is RD + 1,
	    T = avl(NK, NV, L, avl(RK, RV, RL, RR, RD), D1) ).


%% avl_delete_first(+Tree:avl, -Out:avl) is semidet.
% Deletes the first element on the tree.
% Fails if the tree is empty.

avl_delete_first(T, T1) :-
        avl_delete_firsy(T, T1, _, _).

%% avl_delete_first(+Tree:avl, -Out:avl, -Key, -Value) is semidet.
% Deletes and returns the first element on the tree.
% Fails if the tree is empty.

avl_delete_first(avl(K, V, L, R, _), T1, K1, V1) :-
        avl_delete_first(L, R, K, V, T1, K1, V1).

avl_delete_first(t, R, K, V, R, K, V).
avl_delete_first(avl(LK, LV, LL, LR, _), R, K, V, T1, K1, V1) :-
        (   R = avl(_,_,_,_, RD)
        ->  avl_delete_first(LL, LR, LK, LV, LT1, K1, V1),
            avl_cmp_depth(LT1, RD, Diff),
            (   Diff = 2
            ->  avl_balance_right(K, V, LT1, R, T1)
            ;   D1 is 1 + max(RD, RD - Diff),
                T1 = avl(K, V, LT1, R, D1) )
        ;   K1 = LK,
            V1 = LV,
            T1 = avl(K, V, t, t, 1)).


%% avl_delete_last(+Tree:avl, -Out:avl) is semidet.
% Deletes the last element on the tree.
% Fails if the tree is empty.

avl_delete_last(T, T1) :-
        avl_delete_last(T, T1, _, _).

%% avl_delete_last(+Tree:avl, -Out:avl, -Key, -Value) is semidet.
% Deletes and returns the last element on the tree.
% Fails if the tree is empty.

avl_delete_last(avl(K, V, L, R, _), T1, K1, V1) :-
        avl_delete_last(R, L, K, V, T1, K1, V1).

avl_delete_last(t, L, K, V, L, K, V).
avl_delete_last(avl(RK, RV, RL, RR, _), L, K, V, T1, K1, V1) :-
        (   L = avl(_,_,_,_, LD)
        ->  avl_delete_last(RR, RL, RK, RV, RT1, K1, V1),
            avl_cmp_depth(RT1, LD, Diff),
            (   Diff = 2
            ->  avl_balance_left(K, V, L, RT1, T1)
            ;   D1 is 1 + max(LD, LD - Diff),
                T1 = avl(K, V, L, RT1, D1) )
        ;   K1 = RK,
            V1 = RV,
            T1 = avl(K, V, t, t, 1)).

%% avl_delete(+Tree:avl, +Key, -Out:avl) is semidet.
% deletes the element with the given key from the tree.

avl_delete(T, K, T1) :-
        avl_delete(T, K, T1, _).

%% avl_delete(+Tree:avl, +Key, -Out:avl, -Value) is semidet.
% deletes the element with the given key from the tree and return the
% value it had associated.

avl_delete(avl(NK, NV, L, R, _), K, T1, V1) :-
        compare(O, K, NK),
        avl_delete(O, K, NK, NV, L, R, T1, V1).

avl_delete(=, _, _, NV, L, R, T1, NV) :-
        avl_depth(L, LD),
        (   (   avl_cmp_depth(R, LD, 1),
                avl_delete_last(L, L1, DK, DV)
            ->  R1 = R
            ;   avl_delete_first(R, R1, DK, DV),
                L1 = L )
        ->  avl_depth(L1, L1D),
            avl_depth(R1, R1D),
            T1D is 1 + max(L1D, R1D),
            T1 = avl(DK, DV, L1, R1, T1D)
        ;   T1 = t ).

avl_delete(<, K, NK, NV, L, R, T1, V1) :-
        avl_delete(L, K, L1, V1),
        avl_depth(R, RD),
        avl_cmp_depth(L1, RD, Diff),
        (   Diff = 2
        ->  avl_balance_right(NK, NV, L1, R, T1)
        ;   T1D is 1 + max(RD, RD - Diff),
            T1 = avl(NK, NV, L1, R, T1D)).

avl_delete(>, K, NK, NV, L, R, T1, V1) :-
        avl_delete(R, K, R1, V1),
        avl_depth(L, LD),
        avl_cmp_depth(R1, LD, Diff),
        (   Diff = 2
        ->  avl_balance_left(NK, NV, L, R1, T1)
        ;   T1D is 1 + max(LD, LD - Diff),
            T1 = avl(NK, NV, L, R1, T1D)).

%% avl_delete_list(+Tree:avl, +Keys:list, -Out:avl) is semidet.
% delete from the tree all the elements with keys in Keys.

avl_delete_list(T, L, T1) :-
        avl_delete_list1(L, T, T1, _).

%% avl_delete_list(+Tree:avl, +Keys:list, -Out:avl, -Vals:list) is semidet.
% delete from the tree all the elements with keys in Keys and returns
% a list with the values of the deleted entries.

avl_delete_list(T, L, T1, V1) :-
        avl_delete_list1(L, T, T1, V1).

avl_delete_list1([], T, T, []).
avl_delete_list1([K|L], T, T1, [V|VL]) :-
        avl_delete(T, K, T2, V),
        avl_delete_list1(L, T2, T1, VL). 

%% avl_has(+Tree:avl, +Key) is semidet.
% Successes when the tree contains an entry with the given key.

avl_has(T, K) :-
	avl_get(T, K, _).


%% avl_get(+Tree:avl, +Key, -Value) is semidet.
% Retrieves the value associated to the given key.

avl_get(t(NK, NV, L, R, _), K, V) :-
	compare(O, K, NK),
	avl_get(O, K, NV, L, R, V).
avl_get(=, _,V,_,_,V).
avl_get(<,K,_,L,_,V) :-
	avl_get(L, K, V).
avl_get(>,K,_,_,R,V) :-
	avl_get(R, K, V).


%% list_to_avl(+List:list, -Out:avl) is det.
% Converts a list of Key-Value pairs to an AVL tree.
%
% If the list contains several pairs with the same key, the value
% stored on the tree will be that of the last pair.
%
% This operation has complexity O(N*log N).
%
% @param List list of pairs [K1-V1, K2-V2, ...]

list_to_avl(L, O) :-
	list_to_avl(L, t, O).

list_to_avl([], O, O).
list_to_avl([K-V|L], T, O) :-
	avl_replace(T, K, V, T1),
	list_to_avl(L, T1, O).

%% keys_to_avl(L:list, -Out:avl) is det.
% Creates a tree with the keys in the given list and all the values
% [].

keys_to_avl(L, O) :-
	keys_to_avl(L, t, O).

keys_to_avl([], O, O).
keys_to_avl([K|L], T, O) :-
	avl_replace(T, K, [], T1),
	keys_to_avl(L, T1, O).


%% ordered_list_to_avl(List:list, -Out:avl) is det.
% Creates a tree with the elements in the given list. The list must be
% sorted by key and all the keys must be unique.
%
% This operation has complexity O(N).

ordered_list_to_avl(List, T) :-
	length(List, E),
	ordered_list_to_avl(E, List, [], _, T1),
	T=T1.

ordered_list_to_avl(0, List, Rest, 0, t) :-
	!,
	List=Rest.
ordered_list_to_avl(1, List, Rest, 1, avl(K, V, t, t, 1)) :-
	!,
	List=[K-V|Rest].
ordered_list_to_avl(N, List, Rest, D, avl(K, V, L, R, D)) :-
	A is N//2,
	ordered_list_to_avl(A, List, [K-V|More], D1, L),
	D is D1+1,
	Z is N-1-A,
	ordered_list_to_avl(Z, More, Rest, _, R).

  
%% avl_gen(+Tree:avl, -K, -V) is nondet.
% enumerates the elements on the tree via backtracking.

avl_gen(t(_,_,L,_,_), K, V) :-
	avl_gen(L, K, V).
avl_gen(t(K,V,_,_,_), K, V).
avl_gen(t(_,_,_,R,_), K, V) :-
	avl_gen(R, K, V).

  
%% avl_dump(+Tree) is det.
% prints an human friendly representation of the tree to the current stream.
%
% @tbd use portray/1 instead.

avl_dump(T) :-
	avl_dump(T, '').
avl_dump(t, S) :-
	format('~pt~n', [S]).
avl_dump(avl(K, V, L, R, D), S) :-
	format('~pavl ~p=~p (~p)~n', [S, K, V, D]),
	atom_concat(S, '   |', SL),
	avl_dump(L, SL),
	atom_concat(S, '    ', SR),
	avl_dump(R, SR).


%% avl_merge(+Tree1:avl, +Tree2:avl, -Out) is det.
% Merges two AVL trees into one.
%
% When entries with the same key exist on both trees the one from the
% second one is picked.
%
% This operation has complexity O(N1+N2).

avl_merge(T1, T2, TO) :-
        avl_to_list(T1, L1),
        avl_to_list(T2, L2),
        merge_sorted_lists(L1, L2, LO),
        sorted_list_to_avl(LO, TO).

merge_sorted_lists(L1, L2, LO) :-
        (   L1 = []
        ->  L2 = LO
        ;   (   L2 = []
            ->  LO = L1
            ;   merge_sorted_lists1(L1, L2, LO))).

merge_sorted_lists1(L1, L2, LO) :-
        L1 = [H1|T1],
        L2 = [H2|T2],
        H1 = K1-_,
        H2 = K2-_,
        (   K1 @< K2
        ->  LO = [H1|LO1],
            (   T1 = []
            ->  LO1 = L2
            ;   merge_sorted_lists1(T1, L2, LO1))
        ;   LO = [H2|LO1],
            (   K1 @> K2
            ->  (   T2 = []
                ->  LO1 = L1
                ;   merge_sorted_lists1(L1, T2, LO1))
            ;   merge_sorted_lists(T1, T2, LO1))).


%% avl_small_merge(+Tree1:avl, +Tree2:avl, -Out:avl) is det.
% Merges two AVL trees into one.
%
% This operation has complexity O(N2*log(N1+N2)).

avl_small_merge(T1, T2, TO) :-
        avl_to_list(T2, L),
        avl_replace_list1(L, T1, TO).

%% avl_iterator(+Tree:avl, -Iterator:avl_iterator) is det.
% Creates an iterator for the given tree.

avl_iterator(T, avli(T, [])).

%% avl_iterator_advance(+Iterator:avl_iterator, -Out:avl_iterator, -Key) is semidet.
% Advances and returns the key from the next element from the tree
% iterator.
%
% Fails when the iterator surpasses the end of the tree.

avl_iterator_advance(I, I1, K) :-
        avl_iterator_advance(I, I1, K, _).

%% avl_iterator_advance(+Iterator:avl_iterator, -Out:avl_iterator, -Key, -Value) is semidet.
% Advances and returns the next element from the tree iterator.
%
% Fails when the iterator surpasses the end of the tree.

avl_iterator_advance(avli(T, W), avli(T1, W1), K, V) :-
        avl_iterator_advance(T, W, T1, W1, K, V).

avl_iterator_advance(T, W, T1, W1, K, V) :-
        (   T =  avl(_, _, L, _, _)
        ->  avl_iterator_advance(L, [T|W], T1, W1, K, V)
        ;   W = [avl(K, V, _, T1, _)|W1]).


%% avl_break(+Tree:avl, -Key, -Value, -Left:avl, -Right:avl) is semidet
% Inspects AVL tree internals returning the Key/Value pair at the root
% element and the left and right subtrees.

avl_break(avl(K, V, L, R, _), K, V, L, R).


%% avl_map(+Tree:avl, +Op) is semidet.
% Calls Op/2 on all the elements of the tree as Op(K, V).
%
% Fails if any of the calls fail.

:- meta_predicate avl_map(+, 2).
:- meta_predicate avl_map(+, 3, -).

avl_map(t, _).
avl_map(avl(K, V, L, R, _), Op) :-
        avl_map(L, Op),
        call(Op, K, V),
        avl_map(R, Op).

%% avl_map(+Tree:avl, +Op, -Out:avl) is semidet.
% Calls Op/3 on all the elements of the tree as Op(K, V, V1) and returs
% a new tree with the K-V1 mapping.
%
% Fails if any of the calls to Op fail.

avl_map(t, _, t).
avl_map(avl(K, V, L, R, D), Op, avl(K, V1, L1, R1, D)) :-
        avl_map(L, Op, L1),
        call(Op, K, V, V1),
        avl_map(R, Op, R1).

