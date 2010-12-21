% -*- Mode: Prolog -*-

/** <module> List shuffling library
  
  Copyright (C) 2010 by Salvador Fandiño

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

:- module(shuffle, [shuffle/2]).

%% shuffle(+List:list, -Shuffled:list) is det.
% Shuffles the given list returning a new list with the same elements
% in some random order.
%
% The algorithm used has complexity O(N*log N).

shuffle(L, Out) :-
        length(L, Len),
        (   Len > 1
        ->  shuffle(L, [], Len, Out)
        ;   Out = L).

shuffle(L, Tail, Len, Out) :-
        /* format('shuffling ~w~n', [L]), */
        (   Len > 1
        ->  Len1 is Len // 2,
            Len2 is Len - Len1,
            divide_list(L, Len1, Len2, L1, L2),
            shuffle(L2, Tail, Len2, SL2),
            shuffle(L1, SL2, Len1, Out)
            /* format('shuffled ~w ==> ~w~n', [L, Out]) */
        ;   L = [H|_],
            Out = [H|Tail]).

divide_list([H|T], Len1, Len2, L1, L2) :-
        (   Len1 > random(Len1 + Len2)
        ->  L1 = [H|T1],
            (   Len1 = 1
            ->  L2 = T
            ;   succ(Len11, Len1),
                divide_list(T, Len11, Len2, T1, L2))
        ;   L2 = [H|T2],
            (   Len2 = 1
            ->  L1 = T
            ;   succ(Len21, Len2),
                divide_list(T, Len1, Len21, L1, T2))).
