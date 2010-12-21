% -*- Mode: Prolog -*-

/** <module> Hashtables for prolog

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
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  US

  @author Salvador Fandiño
  @license GPL

*/

:- module(hashtable,
          [ ht_new/1,
            ht_empty/1,
            ht_to_list/2,
            ht_keys/2,
            ht_values/2,
            ht_put/3,
            ht_put/2,
            ht_replace/2,
            ht_replace/3,
            ht_delete/2,
            ht_delete/3,
            ht_put_list/2,
            ht_replace_list/2,
            ht_put_keys/2,
            ht_put_keys/3,
            ht_replace_keys/2,
            ht_replace_keys/3,
            ht_get/3,
            ht_has/2,
            ht_reserve/2,
            ht_size/2,
            list_to_ht/2,
            keys_to_ht/2,
            keys_to_ht/3,
	    ht_stats/1,
            ht_gen/3,
            ht_map/2,
            ht_map/3]).

/** ht_new(-HT:ht) is det.

  Creates a new hastable.

*/
ht_new(HT) :-
        ht_new(HT, 16).

/** ht_new(-HT:ht, +Size:int) is det.

  Creates a new hashtable structure with space preallocated for Size
  elements.
*/
ht_new(HT, Max) :-
	HT = ht(buckets, 0, 0),
	ht_reserve(HT, Max).

/** ht_empty(+HT:ht) is det.

  Test if the hash is empty
  
*/
ht_empty(ht(_, 0, _)).

/** ht_size(+HT:ht, -Size:int) is det.

  Returns the number of elements stored in the hashtable.
*/
ht_size(ht(_, Size, _), Size).

/** ht_reserve(!HT, +More) is det.

   Preallocates space for More more elements in the hashtable.
*/

ht_reserve(HT, More) :-
        HT = ht(Buckets, Size, Max),
	(   Max < Size + More
        ->  functor(Buckets, _, Arity),
            Arity1 is max(Arity * 2, (Size + More + 3) // 4),
	    functor(Buckets1, buckets, Arity1),
	    init_buckets(1, Buckets1),
	    rellocate_entries(1, Buckets, Buckets1),
	    setarg(1, HT, Buckets1),
	    Max1 is Arity1 * 6,
	    setarg(3, HT, Max1)
        ;   true ).

rellocate_entries(N, Buckets, Buckets1) :-
        (   arg(N, Buckets, E)
        ->  fast_put_list(E, Buckets1),
            succ(N, N1),
            rellocate_entries(N1, Buckets, Buckets1)
        ;   true ).

init_buckets(N, F) :-
        (   setarg(N, F, [])
        ->  succ(N, N1),
            init_buckets(N1, F)
        ;   true ).

fast_put_list([], _).
fast_put_list([K-V|T], Buckets) :-
        fast_put(Buckets, K, V),
        fast_put_list(T, Buckets).

fast_put(Buckets, K, V) :-
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Old),
        setarg(Index, Buckets, [K-V|Old]).

/** ht_to_list(!HT:ht, -List:list_of_pairs) is det.

  Builds a list containing the Key-Value pairs from the hashtable.

  List is of the form [Key1-Value1, Key2-Value2, ...]

*/
ht_to_list(ht(Buckets, _, _), L) :-
        to_list(1, Buckets, L).

to_list(N, Buckets, L) :-
        (   arg(N, Buckets, E)
        ->  append_to_list(E, L, Tail),
            succ(N, N1),
            to_list(N1, Buckets, Tail)
        ;   L = [] ).

append_to_list([], L, L).
append_to_list([K-V|T], [K-V|M], Tail) :-
                                % the K-V frame is duplicated on
                                % purpose so that a later ht_replace/2
                                % operation will not modify the
                                % generated list.
        append_to_list(T, M, Tail).


/** ht_keys(!HT:ht, -Keys:list) is det.

  Returns a list with the keys on the hash.

*/

ht_keys(ht(B, _, _), L) :-
        keys(1, B, L).

keys(N, B, L) :-
        (   arg(N, B, E)
        ->  append_keys(E, L, T),
            succ(N, N1),
            keys(N1, B, T)
        ;   L = []).

append_keys([], L, L).
append_keys([K-_|T], [K|T1], T2) :-
        append_keys(T, T1, T2).

ht_values(ht(B, _, _), L) :-
        values(1, B, L).

/** ht_keys(!HT:ht, -Keys:list) is det.

  Returns a list with the values on the hash.

*/

values(N, B, L) :-
        (   arg(N, B, E)
        ->  append_values(E, L, T),
            succ(N, N1),
            keys(N1, B, T)
        ;   T = []).

append_values([], L, L).
append_values([_-V|T], [V|T1], T2) :-
        append_values(T, T1, T2).

/** list_to_ht(+List:list_of_pairs, -HT:ht) is det.
  
  Builds a new hashtable from the given list.
*/
  
list_to_ht(List, HT) :-
        ht_new(HT),
        ht_replace_list(HT, List).

/** keys_to_ht(+List:list, -HT:ht) is det.

  Builds a new hashtable from the keys in list using [] as the value.
*/
keys_to_ht(List, HT) :-
	ht_new(HT),
	ht_replace_keys(HT, List).

/** keys_to_ht(+List:list, -HT:ht, Value) is det.

  Builds a new hashtable from the keys in list using Value as the value.
*/
keys_to_ht(List, HT, Value) :-
	ht_new(HT),
	ht_replace_keys(HT, List, Value).

/** ht_put_list(!HT:ht, +List:list_of_pairs) is semidet.

  Inserts the Key-Value pairs from List into the hashtable
*/
ht_put_list(HT, List) :-
        length(List, Len),
        ht_reserve(HT, Len),
        put_list(List, HT, fail).

/** ht_replace_list(!HT:ht, +List:list_of_pairs) is det.

  Inserts the Key-Value pairs from List into the hashtable replacing
  any preexistant colliding entry.
*/
ht_replace_list(HT, List) :-
        length(List, Len),
        ht_reserve(HT, Len),
        put_list(List, HT, replace).

put_list([], _, _).
put_list([K-V|T], HT, Replace) :-
        put(HT, K, V, Replace),
        put_list(T, HT, Replace).


/** ht_put_keys(!HT:ht, +List:list) is semidet.
  
  Inserts the keys from List into the hashtable using [] as the value.
*/
ht_put_keys(HT, List) :-
	ht_put_keys(HT, List, []).

/** ht_put_keys(!HT:ht, +List:list, +Value) is semidet.

  Inserts the keys from List into the hashtable using Value as the value.
*/
ht_put_keys(HT, List, Value) :-
	length(List, Len),
	ht_reserve(HT, Len),
	put_keys(List, HT, Value, fail).

put_keys([], _, _, _).
put_keys([K|T], HT, V, Replace) :-
        put(HT, K, V, Replace),
        put_keys(T, HT, V, Replace).

/** ht_replace_keys(!HT:ht, +List:list) is det.

  Inserts the keys from List into the hastable using [] as the
  value. Overwrites any previous colliding entries.
*/

ht_replace_keys(HT, List) :-
        ht_replace_keys(HT, List, []).

/** ht_replace_keys(!HT:ht, +List:list, Value) is det.

  Inserts the keys from List into the hastable with the given
  value. Overwrites any previously existent colliding entries.
*/

ht_replace_keys(HT, List, Value) :-
        length(List, Len),
        ht_reserve(HT, Len),
        put_keys(List, HT, Value, replace).

entry_lookup([H|T], K, Entry) :-
        (   H = K-_
        ->  Entry = H
        ;   entry_lookup(T, K, Entry) ).

bucket_index(Buckets, K, Index) :-
        functor(Buckets, _, Arity),
        term_hash(K, -1, Arity, Hash),
        (   var(Hash)
        ->  instantiation_error(K)
        ;   succ(Hash, Index)).

put(HT, K, V, Replace) :-
        HT = ht(Buckets, Size, _),
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Old),
        (   entry_lookup(Old, K, Entry)
        ->  (   Replace = replace
            ->  setarg(2, Entry, V) )
        ;   setarg(Index, Buckets, [K-V|Old]),
            succ(Size, Size1),
            setarg(2, HT, Size1) ).

/** ht_put(!HT:ht, +Key) is semidet.

  Inserts the pair Key-[] into the hashtable.

  Fails when an entry with the same key already exists.
*/
ht_put(HT, K) :-
        ht_put(HT, K, []).

/** ht_put(!HT:ht, +Key, +Value) is semidet.

  Inserts the pair Key-Value into the hashtable.

  Fails when an entry with the same key already exists.
*/
ht_put(HT, K, V) :-
        ht_reserve(HT, 1),
        put(HT, K, V, fail).

/** ht_replace(!HT:ht, +Key) is det.

  Inserts the pair Key-[] into the hashtable replacing any
  previous entry with the same key.
*/
ht_replace(HT, K) :-
        ht_reserve(HT, 1),
        put(HT, K, [], replace).

/** ht_replace(!HT:ht, +Key, +Value) is det.

  Inserts the pair Key-Value into the hashtable replacing any
  previous entry with the same key.
*/
ht_replace(HT, K, V) :-
        ht_reserve(HT, 1),
        put(HT, K, V, replace).

/** ht_delete(!HT:ht, +Key) is semidet.

  Deletes the entry for the given key from the hash.
*/

ht_delete(HT, K) :-
        ht_delete(HT, K, _).

/** ht_delete(!HT:ht, +Key, -Value) is semidet.

  Deletes the entry for the given key from the hash and returns the
  value that it had.
*/

ht_delete(HT, K, V) :-
        HT = ht(Buckets, Size, _),
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Old),
        delete_entry(Old, K, New, V),
        setarg(Index, Buckets, New),
        succ(Size1, Size),
        setarg(2, HT, Size1).

delete_entry([H|T], K, New, V) :-
        (   H = K-V
        ->  New = T
        ;   New = [H|New1],
            delete_entry(T, K, New1, V)).

/** ht_get(+HT:ht, +Key, -Value) is semidet.

  Retrieves the value associated to the given key
*/

ht_get(ht(Buckets, _, _), K, V) :-
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Entries),
        entry_lookup(Entries, K, K-V).

/** ht_has(+HT:ht, +K) is semidet.

  Succeeds when an entry exists for the given key.
*/
ht_has(HT, K) :-
        ht_get(HT, K, _).

/** ht_gen(+HT:ht, -K, -V) is nondet.
  
  Visits on backtracking all the Key-Value pairs in the hashtable.
*/
ht_gen(ht(Buckets, _), K, V) :-
        gen(1, Buckets, K, V).

gen(N, Buckets, K, V) :-
        (   arg(N, Buckets, E)
        ->  (   gen_list(E, K, V)
            ;   succ(N, N1),
                gen(N1, Buckets, K, V) ) ).

gen_list([K-V|_], K, V).
gen_list([_|T], K, V) :- gen_list(T, K, V).

/** ht_stats(+HT:ht) is det.

  Prints some information about the hashtable.

  This predicate should only be used for debugging and may be changed in the future
*/
ht_stats(ht(B, Size, Max)) :-
	functor(B, _, Arity),
	stats(1, B, 0, LL, 0, Empty), % LL is sum(sqr(length(Entry)))
	Med is Size/Arity,
	Var is LL / Arity - Med * Med,
	Sigma is sqrt(Var),
	writeln(ht_stats(size:Size, max_size:Max, arity:Arity, blen:Med, bvar:Var, bsigma:Sigma, empty:Empty)).

stats(N, B, LL0, LL1, Emp0, Emp1) :-
	(   arg(N, B, E)
        ->  succ(N, N1),
            (   E = []
            ->  succ(Emp0, Emp2),
                stats(N1, B, LL0, LL1, Emp2, Emp1)
            ;   length(E, Len),
                LL2 is LL0 + Len * Len,
                stats(N1, B, LL2, LL1, Emp0, Emp1))
        ;   LL0 = LL1,
            Emp0 = Emp1).


:- meta_predicate ht_map(+, 2).
:- meta_predicate ht_map(+, 3, -).

/** ht_map(+HT:ht, +Op) is semidet.

Calls Op(K, V) for all the elements on the hash.

*/

ht_map(ht(B, _, _), Op) :-
        map_args(1, B, Op).

map_args(N, B, Op) :-
        (   arg(N, B, List)
        ->  map_arg_entries(List, Op),
            succ(N, N1),
            map_args(N1, B, Op)
        ;   true ).

map_arg_entries([], _).
map_arg_entries([K-V|T], Op) :-
        call(Op, K, V),
        map_arg_entries(T, Op).


/** ht_map(+HT:ht, +Op, -Out:ht) is semidet.

Calls Op(K, V, V1) for all the elements on the hash returning a new
hash with the values gnerated.

*/

ht_map(ht(B, Size, Max), Op, Out) :-
        must_be(var, Out),
        Out = ht(B1, Size, Max),
        functor(B, F, A),
        functor(B1, F, A),
        map_args(0, B, Op, B1).
        
map_args(N, B, Op, B1) :-
        (   arg(N, B, L),
            arg(N, B1, L1)
        ->  map_arg_entries(L, Op, L1),
            succ(N, N1),
            map_args(N1, B, Op, B1)
        ;   true ).

map_arg_entries([], _, []).
map_arg_entries([K-V|T], Op, [K-V1|T1]) :-
        call(Op, K, V, V1),
        map_arg_entries(T, Op, T1).
