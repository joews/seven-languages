% the namespace used to call functions from this module
-module(matching_function).

% list of functions (in the form name/arity) to export
-export([number/1]).

% Call this function from the erl shell:
% 1> c(matching_function).
% 2> matching_function:basic(one).
% 1

% Erlang is dynamically typed but arity is strict:
% 11> matching_function:number(four, five).
% ** exception error: undefined function matching_function:number/2

% a function with pattern matching
% the last case is terminated with `.`; others with `;`.
number(one) -> 1;
number(two) -> 2;
number(three) -> 3.

% a default case (since Default will bind to anything)
% number(Default) -> Default.
