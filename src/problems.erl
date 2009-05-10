-module(problems).
-export([test/0]).

% Using this to remind myself of the erlang syntax.
% Working from this list: http://aperiodic.net/phil/scala/s-99/

% P01
last([H|[]]) ->
  H;
last([_|T]) ->
  last(T).

% P02
penultimate([P,_|[]]) ->
  P;
penultimate([_|T]) ->
  penultimate(T).

% P03
nth(1, [Head|_]) ->
  Head;
nth(N, [_|Tail]) ->
  nth(N - 1, Tail).

% P04
listlength([]) ->
  0;
listlength([_|T]) ->
  1 + listlength(T).

% P05
reverse([]) ->
  [];
reverse([H|T]) ->
  reverse(T) ++ [H].

% P06
is_palindrome(List) ->
  reverse(List) == List.

% P07
flatten([]) ->
  [];
flatten([H|T]) ->
  flatten(H) ++ flatten(T);
flatten(AnythingElse) ->
  [AnythingElse].

% P08
compress([]) ->
  [];
compress([H,H|T]) -> compress([H|T]);
compress([H|T]) -> [H] ++ compress(T).


% P09
pack([]) -> [];
pack([H|T]) -> pack_duplicates(H, [H], T).

pack_duplicates(D, DupsSoFar, [D|T]) -> pack_duplicates(D, [D|DupsSoFar], T);
pack_duplicates(_, DupsSoFar, T) -> [DupsSoFar | pack(T)].
  

% P10
encode(List) -> [{listlength(T) + 1, X} || [X|T] <- pack(List)]. 
  
% P11
encode_modified(List) -> [modified_encoded_element(X) || X <- encode(List)].

modified_encoded_element({1, E}) -> E;
modified_encoded_element(Any) -> Any.

% P12

decode(List) -> flatten([decode_element(E) || E <- List]).

decode_element({1, E}) -> E;
decode_element({N, E}) -> [E|decode_element({N - 1, E})].

% P13
encode_direct([E,E|T]) ->
  encode_duplicates({2, E}, T);
encode_direct([H|T]) ->
  [{1, H}|encode_direct(T)];
encode_direct(Any) ->
  Any.

encode_duplicates({Number, E}, [E|T]) ->
  encode_duplicates({Number + 1, E}, T);
encode_duplicates(Tuple, T) ->
  [Tuple|encode_direct(T)].

% P14
duplicate([H|T]) -> [H,H|duplicate(T)];
duplicate([]) -> [].

% P15
duplicateN(N, [H|T]) -> makeN	(N, H) ++ duplicateN(N, T);
duplicateN(_, []) -> [].

makeN(1, E) -> [E];
makeN(N, E) -> [E|makeN(N-1, E)].

% P16
dropNth(N, List) -> reverse(dropAt(N, 1, List, [])).

dropAt(At, At, [_|T], Result) -> dropAt(At, 1, T, Result);
dropAt(_, _, [], Result) -> Result;
dropAt(At, Current, [H|T], Result) -> dropAt(At, Current + 1, T, [H|Result]).
 

% P17
split(0, List) -> {[], List};
split(_, []) -> {[],[]};
split(N, List) -> splitIntoTwo(1, N, [], List).

splitIntoTwo(FirstLength, FirstLength, FirstHalf, [H|T]) -> {FirstHalf ++ [H], T};
splitIntoTwo(Current, FirstLength, FirstHalf, [H|T]) -> splitIntoTwo(Current + 1, FirstLength, FirstHalf++ [H], T).

% All done; here are the tests
test() ->
  X = ["A", "B", "C", "D", "E", "F", "G"],
  test_last(X),
  test_penultimate(X),
  test_nth(X),
  test_length(X),
  test_is_palindrome(),
  test_flatten(X),
  test_compress(),
  test_pack(),
  test_encode(),
  test_encode_modified(),
  test_decode(),
  test_encode_direct(),
  test_duplicate(),
  test_duplicateN(),
  test_dropNth(),
  test_split(),
  io:format("Tests passed.~n"),
  init:stop().

test_last(List) ->
 "G" = last(List).

test_penultimate(List) ->
  "F" = penultimate(List).

test_nth(List) ->
  "F" = nth(6, List).

test_is_palindrome() ->
  false = is_palindrome([1,2,3,4]),
  true = is_palindrome([1,2,3,2,1]).

test_flatten(List) ->
  "ABCDEFG" = flatten(List).

test_length(List) ->
  7 = listlength(List).

test_compress() ->
  ['a','b', 'c', 'd'] = compress(['a', 'b', 'b', 'c', 'c', 'c', 'c', 'd']).

test_pack() ->
  [[a], [b,b], [c, c, c], [d]] = pack([a, b, b, c, c, c, d]).

test_encode() ->
  [{1,a}, {2, b}, {3, c}, {1,d}] = encode([a, b, b, c, c, c, d]).

test_encode_modified() ->
  [a, {2, b}, {3, c}, d] = encode_modified([a, b, b, c, c, c, d]).

test_decode() ->
  [a, b, b, c, c, c, d] = decode([{1,a}, {2, b}, {3, c}, {1,d}]).

test_encode_direct() ->
  [{1,a}, {2, b}, {3, c}, {1,d}] = encode_direct([a, b, b, c, c, c, d]).

test_duplicate() ->
  [a,a,b,b,b,b,c,c,d,d] = duplicate([a, b, b, c, d]).

test_duplicateN() ->
  [a,a,a,b,b,b,b,b,b,c,c,c,d,d,d] = duplicateN(3, [a, b, b, c, d]).

test_dropNth() ->
  [a,b,d,e,g,h,j] = dropNth(3, [a,b,c,d,e,f,g,h,i,j]).

test_split() ->
  {[a,b,c], [d,e,f,g]} = split(3, [a,b,c,d,e,f,g]).