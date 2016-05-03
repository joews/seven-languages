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


