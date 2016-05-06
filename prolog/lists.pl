% Two tuples can unify if they are the same length and 
%  each element can unify:
% (1, 2, 3) = (1, 2, 3) % yes
% (A, 2, 3) = (1, B, C) % yes
% (1, 1, 2) = (X, X, Y) % yes
% (1, 2, 2) = (X, X, Y) % no

% Lists follow the same rule for unification.
% Lists can be destructured with Head|Tail:
% [Head|Tail] = [1, 2, 3] % Head = 1, Tail = [2, 3]

% An empty list does not destructure, but a single element list does
% [Head|Tail] = [] % no
% [Head|Tail] = [1] % Head = 1, Tail = []

% Like atoms and variables, destructured lists can unify anywhere:
% [a, 1, 2] = [a|Tail] % yes
% [a, b, c] = [a, Head|Tail] % Head = b, Tail = [c]
% [a, b, c] = [First|[Second|[Third]]] % First = a, Second = b, Third = c
% [a, b, c, d, e] = [_, _|[Third|_]]. % Third = c

% Declarative list algorithms 
count(0, []).
count(Count, [Head|Tail]) :- 
  count(TailCount, Tail),
  Count is TailCount + 1.

sum(0, []).
sum(Sum, [Head|Tail]) :-
  sum(TailSum, Tail),
  Sum is TailSum + Head.


mean(0, []).
mean(Mean, List) :-
  sum(Sum, List),
  count(Count, List),
  Mean is Sum / Count.

% re-implementation of prolog's `append`:  
concat([], List, List).

% Special cases where list 1 has 1, 2 and 3 elements...
concat([Head|[]], List, [Head|List]).
concat([Head1|[Head2|[]]], List, [Head1, Head2|List]).
concat([Head1|[Head2|[Head3|[]]]], List, [Head1, Head2, Head3|List]).

% ...which generalises to this!
concat2([], List, List).
concat2([Head|Tail1], List, [Head|Tail2]):- 
  concat2(Tail1, List, Tail2).

% concat/append has superpowers.

% "equal-if-concatenated" test:
concat([a], [b], [a, b]). % true
concat([a], [b], [a, b, c]). % no

% concatenate lists:
concat([a, b], [c, d], L). % L = [a, b, c, d]

% list difference:
concat([a], Diff, [a, b, c]). % Diff = [b, c]

% compute splits:
concat(A, B, [a, b, c, d]). % A, B: all split permutations, e.g:
% A = [], B = [a,b,c,d] ? a
% A = [a], B = [b,c,d]
% A = [a,b], B = [c,d]
% A = [a,b,c], B = [d]

%
%  Some more list functions
%
last([H|[]], H).
last([H|T], R) :- last(T, R).

at([H|_], 0, H).
at([H|T], N, R) :-
  NN is N - 1,
  at(T, NN, R).

% reverse is built-in
rev([H|[]], [H]).
rev([H1|[H2|[]]], [H2, H1]).
rev([H|T], Result) :-
  rev(T, TRev),
  concat(TRev, [H], Result).
