parent(hugh, sue).
parent(sue, andy).
parent(andy, joe).
parent(joe, peach).
parent(joe, plum).

% rules can have several definitions, only one of which must match - logical OR
ancestor(X, Y) :-
  parent(X, Y).

% recursive rules with proper tail calls
ancestor(X, Y) :-
  parent(X, Z),
  ancestor(Z, Y).

% We can query both sides of the relationship, so this
%  relation works for "ancestor" and "descendant".
% | ?- ancestor(joe, peach). % true
% | ?- ancestor(hugh, plum). % true
% | ?- ancestor(hugh, Who).  % sue, andy, joe, peach, plum
% | ?- ancestor(Who, joe).   % hugh, sue, andy


% factorial
% not tail-recursive - factorial(999999, W) causes a local stack overflow.
% factorial(35, W) causes integer overflow.
% factorial(34, W) is ~2^58.
% current_prolog_flag(max_integer, Max) and current_prolog_flag(min_integer, Min) 
%  show that, on this machine, Prolog uses 60bit signed integers.
factorial(0, 1).
factorial(N, Result) :-
  N > 0,                        % precondition
  N1 is N - 1,                   
  factorial(N1, NextFactorial), % recursively match the next step
  Result is N * NextFactorial.  % use the next step to compute result

% properly tail recursive factorial
% the second parameter is an accumulator. factorial2/2 is a convenience wrapper
%  that initialises the accumulator to 1.

% tail recursion eliminates the "local" stack overflow that factorial hits,
%  but factorial2(999999, 1, R) aborts with a global stack overflow at 32mb. In contrast
%  the non-tail-recursive form aborts with a "local" stack overflow at 16mb.
% it seems that the gprolog interpreter doesn't support proper tail calls. 
% the SWI prolog interpreter does, and it seems that compiled gprolog programs
%  also do - h/t comment on http://stackoverflow.com/a/7863217/2806996
factorial2(N, Result) :- factorial2(N, 1, Result).
factorial2(0, A, A).
factorial2(N, A, Result) :-
  A1 is N * A,
  N1 is N - 1,
  factorial2(N1, A1, Result).


% fibonacci
% my solution - not tail recursive
fib(0, 1).
fib(1, 1).
fib(N, Result) :-
  N > 1,
  N1 is N - 1,
  N2 is N - 2,
  fib(N1, Result1),
  fib(N2, Result2),
  Result is Result1 + Result2.

