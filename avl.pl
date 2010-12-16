% -*- Mode: Prolog -*-
/** @info @copyright
  
  Copyright (C) 2004 by Salvador Fandiño (sfandino@@yahoo.com)

  PrologDoc is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  PrologDoc is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with PrologDoc; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  @author(format=svn) $Author: salva $
  @version(format=svn) $Revision: 1.9 $
  @date(format=svn) $Date: 2005-03-20 16:07:24 +0100 (Sun, 20 Mar 2005) $
  @source(format=svn) $URL: file:///tmp/crunc/trunk/prolog/avl/avl.pl $

  */

:- module(avl, [ avl_empty/1,
		 avl_to_list/2,
		 avl_put/4,
		 avl_put/3,
		 avl_replace/4,
		 avl_replace/3,
		 avl_get/3,
		 avl_has/2,
		 sorted_list_to_avl/2,
		 list_to_avl/2,
		 avl_gen/3,
		 avl_dump/1]).

/**
  @s1 Introduction

  This module exports a set of predicates to manipulate AVL trees from Prolog.

    @s2 AVL tree representation

    Empty trees are represented as @c|t|.

    Non empty tree nodes are represented as
      @c|t(Key, Value, Left, Right, Depth)| where

    @list
      @li Key and Value
          are the key/value pair stored on this node.
      @li Left and Right
          are this node child subtrees.
      @li Depth
          is a value used internally to calculate when
          the tree needs to be balanced after an insertion.
    @/list
*/


/**
  @pred avl_empty(?Tree)
    creates an empty AVL tree.
  */
avl_empty(t).

/**
  @pred avl_to_list(+Tree, ?List)
    @arg Tree
        input AVL tree.
    @arg List=[K1-V1, K2-V2, ...]
        output sorted list.

    @desc converts an AVL tree to a sorted list with the @c|Key-Value| pairs.
  */
avl_to_list(T, L) :-
	avl_to_list(T, L, []).

avl_to_list(t, L, L).
avl_to_list(t(K, V, L, R, _), List, Rest) :-
	avl_to_list(L, List, [K-V|M]),
	avl_to_list(R, M, Rest).

/**
  @pred avl_put(+Tree, +Key, -Out)
  equivalent to @c|avl_put(Tree, Key, [], Out)|. See also @rp|avl_put/4|.
  */

avl_put(T, K, T1) :-
	avl_put(T, K, [], T1).


/**
  @pred avl_put(+Tree, +Key, +Value, -Out)
    @arg Tree
        input AVL tree.
    @arg Key, Value
        pair to insert on the AVL tree
    @arg Out
        output tree.

  @desc inserts a @c|Key/Value| pair on an AVL tree. Fails if an
        element with the same @c|Key| already exists on the tree
        (see also @rp|avl_replace/4|).

  */
avl_put(t, K, V, t(K, V, t, t, 1)) :- !.
avl_put(t(NK, NV, L, R, D), K, V, T) :-
	compare(O, K, NK),
	avl_put(O, NK, NV, L, R, D, K, V, T).

avl_put(<, NK, NV, L, R, D, K, V, T) :-
	avl_put(L, K, V, L1),
	(   L1 = t(_, _, _, _, D)
	->  avl_balance_left(NK, NV, L1, R, T)
	;   T = t(NK, NV, L1, R, D) ).

avl_put(>, NK, NV, L, R, D, K, V, T) :-
	avl_put(R, K, V, R1),
	(   R1 = t(_, _, _, _, D)
	->  avl_balance_right(NK, NV, L, R1, T)
	;   T = t(NK, NV, L, R1, D) ).

/**
  @pred avl_replace(+Tree, +Key, -Out)
    equivalent to @c|avl_replace(Tree, Key, [], Out)|.
    See also @rp|avl_replace/4|.
  */

avl_replace(T, K, T1) :-
	avl_replace(T, K, [], T1).

/**
  @pred avl_replace(+Tree, +Key, +Value, -Out)
    @arg Tree
        input AVL tree.
    @arg Key, Value
        pair to insert on the AVL tree
    @arg Out
        output tree.

    @desc inserts a @c|Key/Value| pair on an AVL tree. If an
        element with the same @c|Key| already exists on the tree
        it is replaced (see also @rp|avl_put/4|).
  */

avl_replace(t, K, V, t(K, V, t, t, 1)) :- !.
avl_replace(t(NK, NV, L, R, D), K, V, T) :-
	compare(O, K, NK),
	avl_replace(O, NK, NV, L, R, D, K, V, T).

avl_replace(=, NK, _, L, R, D, _, V, t(NK, V, L, R, D)).
avl_replace(<, NK, NV, L, R, D, K, V, T) :-
	avl_replace(L, K, V, L1),
	(   L1 = t(_, _, _, _, D)
	->  avl_balance_left(NK, NV, L1, R, T)
	;   T = t(NK, NV, L1, R, D) ).

avl_replace(>, NK, NV, L, R, D, K, V, T) :-
	avl_replace(R, K, V, R1),
	(   R1 = t(_, _, _, _, D)
	->  avl_balance_right(NK, NV, L, R1, T)
	;   T = t(NK, NV, L, R1, D) ).

avl_cmp_depth(t, D, D).
avl_cmp_depth(t(_,_,_,_,AD), BD, D) :-
	D is BD - AD.

avl_balance_left(NK, NV, t(LK, LV, LL, LR, LD), R, T) :-
	(   avl_cmp_depth(R, LD, 2)
	->  % avl_dump(t(NK, NV, t(LK, LV, LL, LR, LD),R, _), 'lb: '),nl,
	    (	LR = t(LRK, LRV, LRL, LRR, LRD),
		avl_cmp_depth(LL, LRD, 1)
	    ->	T = t(LRK, LRV, t(LK, LV, LL, LRL, LRD), t(NK, NV, LRR, R, LRD), LD)
	    ;	ND1 is LD-1,
		T = t(LK, LV, LL, t(NK, NV, LR, R, ND1), LD) )
	;   D1 is LD + 1,
	    T = t(NK, NV, t(LK, LV, LL, LR, LD), R, D1) ).
	
avl_balance_right(NK, NV, L, t(RK, RV, RL, RR, RD), T) :-
	(   avl_cmp_depth(L, RD, 2)
	->  % avl_dump(t(NK, NV, L, t(RK, RV, RL, RR, RD), _), 'rb: '),nl,
	    (	RL = t(RLK, RLV, RLL, RLR, RLD),
		avl_cmp_depth(RR, RLD, 1)
	    ->	T = t(RLK, RLV, t(NK, NV, L, RLL, RLD), t(RK, RV, RLR, RR, RLD), RD)
	    ;	ND1 is RD-1,
		T = t(RK, RV, t(NK, NV, L, RL, ND1), RR, RD) )
	;   D1 is RD + 1,
	    T = t(NK, NV, L, t(RK, RV, RL, RR, RD), D1) ).

/**
  @pred avl_has(+Tree, +Key)
    checks whether the AVL tree contains an element with the given key.
 */
avl_has(T, K) :-
	avl_get(T, K, _).

/**
  @pred avl_get(+Tree, +Key, ?Value)
    @arg Tree
        input AVL tree
    @arg Key
        key for the element that wants to be retrieved
    @arg Value
        value found

    @desc retrieves the value associated to some key. Predicate fails if no element with such key is found on the tree.
  */
avl_get(t(NK, NV, L, R, _), K, V) :-
	compare(O, K, NK),
	avl_get(O, K, NV, L, R, V).
avl_get(=, _,V,_,_,V).
avl_get(<,K,_,L,_,V) :-
	avl_get(L, K, V).
avl_get(>,K,_,_,R,V) :-
	avl_get(R, K, V).

/**
  @pred list_to_avl(+List, -Tree)
    @arg List=[K1-V1, K2-V2,...]
        input list to be converted to an AVL tree.
    @arg Tree
        output AVL tree.

    @desc converts a list of @c|Key-Value| pairs to and AVL tree.
          Internally, it uses @rp|avl_replace/4|, so if the list contains
          several elements with the same key, the first ones are
          effectively ignored.
  */
list_to_avl(L, O) :-
	list_to_avl(L, t, O).

list_to_avl([], O, O).
list_to_avl([K-V|L], T, O) :-
	avl_replace(T, K, V, T1),
	list_to_avl(L, T1, O).

/**
  @pred sorted_list_to_avl(+List, -Tree)
    @arg List=[K1-V1, K2-V2, ...]
        input list to be converted to an AVL tree.
        It has to be a sorted and elements keys have to be unique.
    @arg Tree
        output tree.
  
    @desc converts a sorted list of @c|Key-Value| pairs without key duplicates
        to an AVL tree efficiently.
  */
sorted_list_to_avl(List, T) :-
	length(List, E),
	sorted_list_to_avl(E, List, [], _, T1),
	T=T1.

sorted_list_to_avl(0, List, Rest, 0, t) :-
	!,
	List=Rest.
sorted_list_to_avl(1, List, Rest, 1, t(K, V, t, t, 1)) :-
	!,
	List=[K-V|Rest].
sorted_list_to_avl(N, List, Rest, D, t(K, V, L, R, D)) :-
	A is N//2,
	sorted_list_to_avl(A, List, [K-V|More], D1, L),
	D is D1+1,
	Z is N-1-A,
	sorted_list_to_avl(Z, More, Rest, _, R).

/**
  @pred avl_gen(+Tree, ?Key, ?Value)
    @arg Tree
        input AVL tree.
    @arg Key, Value
        pair on the tree.
  
    @desc enumerates via backtracking the elements on the AVL tree.
  */
avl_gen(t(_,_,L,_,_), K, V) :-
	avl_gen(L, K, V).
avl_gen(t(K,V,_,_,_), K, V).
avl_gen(t(_,_,_,R,_), K, V) :-
	avl_gen(R, K, V).

/**
  @pred avl_dump(+Tree)
    prints an human friendly representation of the tree to the current stream.

    TODO: use @l|portray/1| instead.
  */

avl_dump(T) :-
	avl_dump(T, '').
avl_dump(t, S) :-
	format('~pt~n', [S]).
avl_dump(t(K, V, L, R, D), S) :-
	format('~pavl ~p=~p (~p)~n', [S, K, V, D]),
	atom_concat(S, '   |', SL),
	avl_dump(L, SL),
	atom_concat(S, '    ', SR),
	avl_dump(R, SR).

