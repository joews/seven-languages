-module(recursion).
-export([factorial/1]).
-export([fib/1]).
-export([fib2/1]).

% erlang has proper tail calls,
% so this doesn's blow the stack (and is very fast) even with large N.
factorial(0) -> 0;
factorial(1) -> 1;
factorial(N) -> N * factorial(N - 1).

% this is not tail recursive.
% it is very slow and memory intensive even for N=40.
fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N - 2) + fib(N - 1).

% tail-recursive form
% export a friendly API
fib2(N) -> fib2(N, 0, 1).

% implement with two accumulator arguments
fib2(1, _, N) -> N;
fib2(N, F1, F2) -> fib2(N -1, F2, F1 + F2).
