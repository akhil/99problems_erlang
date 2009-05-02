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




% All done; here are the tests
test() ->
  X = ["A", "B", "C", "D", "E", "F", "G"],
  test_last(X),
  test_penultimate(X),
  test_nth(X),
  test_length(X),
  test_is_palindrome(),
  test_flatten(X),
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
