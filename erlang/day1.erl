-module(day1).
-export([word_count/1]).
-export([count_to/1]).
-export([status/1]).

% Exercise: count words in a string
% this is actually a stdlib function: string.words(Str)
% - https://github.com/erlang/otp/blob/3b7a6ffddc819bf305353a593904cea9e932e7dc/lib/stdlib/src/string.erl#L315
% The stdlib implementation uses features I don't know yet.
word_count("") -> 0;

% this feels clunky! a str/sub_str solution is possible
%  with in-function pattern matching.
word_count(Str) ->
  [_|Tail] = string:tokens(Str, " "),
  1 + word_count(string:join(Tail, " ")).


% Exercise: Count from 1 to N
count_to(N) -> count_to(N, 1).

count_to(1, N) -> N;
count_to(N, C) ->
  io:format("~b~n", [C]),
  count_to(N - 1, C + 1).

% Exercise: use pattern matching to return a "success" or error
%  message depending on the input
status(success) -> "success";
status({ error, Message }) -> "error: " ++ Message.
