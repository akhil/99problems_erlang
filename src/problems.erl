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
pack_duplicates(D, DupsSoFar, T) -> [DupsSoFar | pack(T)].
  

% P10
encode(List) -> [[listlength(T) + 1, X] || [X|T] <- pack(List)]. 
  
  
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
  exit(0).

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
  [[1,a], [2, b], [3, c], [1,d]] = encode([a, b, b, c, c, c, d]).