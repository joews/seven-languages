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
% not tail-recursive - factorial(999999, W) causes a stack overflow on this machine.
% factorial(35, W) causes integer overflow.
% factorial(34, W) is ~2^58.
% current_prolog_flag(max_integer, Max) and current_prolog_flag(min_integer, Min) 
%  show that, on this machine, Prolog uses 60bit signed integers.
factorial(0, 1).
factorial(N, Result) :-
  N > 0,                           % precondition
  N1 is N - 1,                   
  factorial(N1, NextFactorial), % recursively match the next step
  Result is N * NextFactorial.     % use the next step to compute result

% properly tail recursive factorial
% the second parameter is an accumulator
% it should be set to 1 at initial entry, but we can't
%  add a precondition because it's called recursively

% Even though this is tail recursive, it overflows!
% TODO: find out why!
factorial2(0, F, F).
factorial2(N, A, Result) :-
  N > 0,
  A1 is N * A,
  N1 is N - 1,
  factorial2(N1, A1, Result).

% A cleaner api for factorial 2
factorial3(N, Result) :- factorial2(N, 1, Result).
