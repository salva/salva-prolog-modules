% -*- Mode: Prolog -*-

/** <module> Hashtables for prolog

  @author Salvador Fandiño
  @license GPL

*/

:- module(hashtable,
          [ ht_new/1,
            ht_empty/1,
            ht_to_list/2,
            ht_put/3,
            ht_put/2,
            ht_put_list/2,
            ht_replace/3,
            ht_get/3,
            ht_has/2,
            ht_reserve/2,
            ht_size/2,
            list_to_ht/2,
	    ht_stats/1,
            ht_gen/3 ]).

/** ht_new(-HT) is det.
  
*/
ht_new(HT) :-
        ht_new(HT, 16).

/** ht_new(-HT, +Size) is det.

  creates a new hashtable structure with space preallocated for Size
  elements.
*/
ht_new(HT, Max) :-
	HT = ht(buckets, 0, 0),
	ht_reserve(HT, Max).

/** ht_empty(+HT) is det.

*/
ht_empty(ht(_, 0, _)).

/** ht_size(+HT, -Size) is det.

  returns the number of elements stored in the hashtable.
*/
ht_size(ht(_, Size, _), Size).

/** ht_reserve(!HT, +More) is det.

   preallocates space for More more elements in the hashtable.
*/

ht_reserve(HT, More) :-
        HT = ht(Buckets, Size, Max),
	(   Max < Size + More
        ->  functor(Buckets, _, Arity),
            Arity1 is max(Arity * 2, (Size + More + 3) // 4),
	    functor(Buckets1, buckets, Arity1),
	    init_buckets(Buckets1, Arity1),
	    rellocate_entries(Buckets, Arity, Buckets1),
	    setarg(1, HT, Buckets1),
	    Max1 is Arity1 * 6,
	    setarg(3, HT, Max1)
        ;   true ).

rellocate_entries(Buckets, Index, Buckets1) :-
        (   Index > 0
        ->  arg(Index, Buckets, List),
            fast_put_list(List, Buckets1),
            Index1 is Index - 1,
            rellocate_entries(Buckets, Index1, Buckets1)
        ;   true ).

init_buckets(F, Index) :-
        (   Index > 0
        ->  setarg(Index, F, []),
            Index1 is Index - 1,
            init_buckets(F, Index1)
        ;   true ).

fast_put_list([], _).
fast_put_list([K-V|T], Buckets) :-
        fast_put(Buckets, K, V),
        fast_put_list(T, Buckets).

fast_put(Buckets, K, V) :-
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Old),
        setarg(Index, Buckets, [K-V|Old]).

/** ht_to_list(!HT, -List) is det.

  builds a list containing the Key-Value pairs from the hashtable.

  List is of the form [Key1-Value1, Key2-Value2, ...]

*/
ht_to_list(ht(Buckets, _, _), L) :-
        functor(Buckets, _, Arity),
        buckets_to_list(Arity, Buckets, L).

/** list_to_ht(+List, -HT) is det.
  
  builds a new hashtable from the given list.
*/
  
list_to_ht(List, HT) :-
        ht_new(HT),
        ht_put_list(HT, List).

/** key_list_to_ht(+List, -HT) is det.

  builds a new hashtable from the keys in list using [] as the value.
*/
key_list_to_ht(List, HT) :-
	ht_new(HT),
	ht_put_key_list(HT, List).

/** ht_put_list(!HT, +List) is det.

  inserts the Key-Value pairs from List into the hashtable
*/
ht_put_list(HT, List) :-
        length(List, Len),
        ht_reserve(HT, Len),
        put_list(List, HT).

put_list([], _).
put_list([K-V|T], HT) :-
        put(HT, K, V, croak),
        put_list(T, HT).

/** ht_put_key_list(!HT, +List) is det.
  
  inserts the keys from List into the hashtable using [] as the value.
*/
ht_put_key_list(HT, List) :-
	ht_put_key_list(HT, List, []).

/** ht_put_key_list(!HT, +List, +Value) is det.

  inserts the keys from List into the hashtable using Value as the value.
*/
ht_put_key_list(HT, List, Value) :-
	length(List, Len),
	ht_reserve(HT, Len),
	put_key_list(List, HT, Value).

put_key_list([], _, _).
put_key_list([K|KT], HT, Def) :-
	put(HT, K, Def),
	put_key_list(KT, HT, Def).

buckets_to_list(Index, Buckets, L) :-
        (   Index > 0
        ->  arg(Index, Buckets, BList),
            append_to_list(BList, L, Tail),
            Index1 is Index - 1,
            buckets_to_list(Index1, Buckets, Tail)
        ;   L = [] ).

% the K-V frame is duplicated on purpose so that a later ht_replace/2
% operation will not modify the generated list.
append_to_list([], L, L).
append_to_list([K-V|T], [K-V|M], Tail) :-
        append_to_list(T, M, Tail).

entry_lookup([H|T], K, Entry) :-
        (   H = K-_
        ->  Entry = H
        ;   entry_lookup(T, K, Entry) ).

bucket_index(Buckets, K, Index) :-
        functor(Buckets, _, Arity),
        term_hash(K, -1, Arity, Hash),
        (   var(Hash)
        ->  throw(error(instantiation_error, _))
        ;   Index is Hash + 1 ).

put(HT, K, V, Replace) :-
        HT = ht(Buckets, Size, _),
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Old),
        (   entry_lookup(Old, K, Entry)
        ->  (   Replace = replace
            ->  setarg(2, Entry, V) )
        ;   setarg(Index, Buckets, [K-V|Old]),
            Size1 is Size + 1,
            setarg(2, HT, Size1) ).

/** ht_put(!HT, +Key) is det.

  Inserts the pair Key-[] into the hashtable.

  Fails when an entry with the same key already exists.
*/
ht_put(HT, K) :-
        ht_put(HT, K, []).

/** ht_put(!HT, +Key, +Value) is det.

  Inserts the pair Key-Value into the hashtable.

  Fails when an entry with the same key already exists.
*/
ht_put(HT, K, V) :-
        ht_reserve(HT, 1),
        put(HT, K, V, fail).

/** ht_put(!HT, +Key, +Value) is det.

   Inserts the pair Key-Value into the hashtable replacing any
   previous entry with the same key.
*/
ht_replace(HT, K, V) :-
        ht_reserve(HT, 1),
        put(HT, K, V, replace).

/** ht_get(+HT, +Key, -Value) is det.

  Retrieves the value associated to the given key
*/

ht_get(ht(Buckets, _, _), K, V) :-
        bucket_index(Buckets, K, Index),
        arg(Index, Buckets, Entries),
        entry_lookup(Entries, K, K-V).

/** ht_has(+HT, +K) is det.

  Succeeds when an entry exists for the given key.
*/
ht_has(HT, K) :-
        ht_get(HT, K, _).

/** ht_gen(+HT, -K, -V)
  Visits on backtracking all the Key-Value pairs in the hashtable.
*/
ht_gen(ht(Buckets, _), K, V) :-
        functor(Buckets, _, Arity),
        gen(Buckets, Arity, K, V).

gen(Buckets, Index, K, V) :-
        (   Index > 0
        ->  arg(Index, Buckets, Entries),
            (   gen_list(Entries, K, V)    
            ;   Index1 is Index - 1,
                gen(Buckets, Index1, K, V) ) ).

gen_list([K-V|_], K, V).
gen_list([_|T], K, V) :- gen_list(T, K, V).

/** ht_stats(+HT) is det.

  Prints some information about the hashtable.

  This predicate should only be used for debugging and may be changed in the future
*/
ht_stats(ht(B, Size, Max)) :-
	functor(B, _, Arity),
	stats(B, Arity, 0-L2, 0-Empty),
	Med is Size/Arity,
	Var is L2 / Arity - Med * Med,
	Sigma is sqrt(Var),
	writeln(ht_stats(size:Size, max_size:Max, arity:Arity, blen:Med, bvar:Var, bsigma:Sigma, empty:Empty)).

stats(B, Index, LL0-LL1, Emp0-Emp1) :-
	(   Index > 0
	->  arg(Index, B, List),
	    length(List, Len),
	    (	Len =:= 0
	    ->	Emp2 is Emp0 + 1
	    ;	Emp2 = Emp0 ),
	    LL2 is LL0 + Len * Len,
	    Index1 is Index - 1,
	    ht_stats(B, Index1, LL2-LL1, Emp2-Emp1)
	;   LL0 = LL1,
	    Emp0 = Emp1).

	