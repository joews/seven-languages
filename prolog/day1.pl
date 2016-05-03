% A knowledge base coprises facts and rules. You ask questions about a knowledge
%  base with queries.

% These are facts
% Values that start with a lowercase character are atoms - immutable values.
likes(wallace, cheese).
likes(gromit, cheese).
likes(wendolene, cheese).

% This is a rule
% It has three subgoals. A query has to match all of them to match the rule.
% It takes two parameters, so we call it friend/2
friend(X, Y) :-
  \+ (X = Y),    % \+ is logical NOT
  likes(X, Z),
  likes(Y, Z).

% Rules are also called clauses. Facts are "unit clauses".
% Facts are equivalent to rules with the body "true":
% friend(a, b).  means  friend(a, b) :- true.

% :- can be read as "if", and commas in rule bodies can be read as "and".

% This is a query. Run it in the REPL to get an answer.
% friend(wallace, gromit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
food_type(gruyere, cheese).
food_type(ritz, cracker).
food_type(spam, meat).
food_type(sausage, meat).
food_type(jolt, soda).
food_type(twinkie, dessert).

flavor(sweet, dessert).
flavor(savory, meat).
flavor(savory, cheese).
flavor(sweet, soda).

food_flavor(X, Y) :- 
  food_type(X, Z), 
  flavor(Y, Z).

% Prolog tries to unify questions with the knowledgebase to fill in variables.
% You can ask questions about facts and rules.
% food_type(gruyere, What).
% food_flavor(gruyere, What).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ask Prolog to choose colours for neighbouring states so that no two adjacent
%  states are the same colour 
different(red, blue). different(red, green).
different(green, red). different(green, blue).
different(blue, red). different(blue, green).

coloring(Alabama, Mississipi, Georgia, Tennessee, Florida) :-
  different(Mississipi, Tennessee),
  different(Mississipi, Alabama),
  different(Alabama, Mississipi),
  different(Alabama, Tennessee),
  different(Alabama, Georgia),
  different(Alabama, Florida),
  different(Georgia, Florida),
  different(Georgia, Tennessee).

% | ?- coloring(A, M, G, T, F).
% 
% A = green
% F = blue
% G = red
% M = red
% T = blue ? ;
%
% A = blue
% F = green
% G = red
% M = red
% T = green ? ;

% etc... (semicolon to see the next solution in the REPL, a to see all)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% moar unification
cat(lion).
cat(tiger).

dorothy(X, Y, Z) :- X = lion, Y = tiger, Z = bear.    
% dorothy(lion, tiger, bear) % yes
% dorothy(lion, tiger, yogi) % no 

% | ?_ dorothy(A, B, C) % unifies A, B, C 
% | ?- dorothy(A, tiger, C). % yes, unifies A, C
% | ?- dorothy(A, bison, C). % no.

both_cats(X, Y) :- cat(X), cat(Y).
% | ?- both_cats(A, B) % gives 4 answers - lion/lion, lion/tiger, tiger/tiger, tiger/lion
% | ?- both_cats(lion, A) % gives two answers

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Exercises
album(radiohead, 'ok computer').
album(radiohead, 'the bends').
album('frank turner', 'love, ire and song').

plays_for(bresnan, yorkshire).
plays_for(rashid, yorkshire).
plays_for(root, yorkshire).
plays_for(ali, warwickshire).
plays_for(broad, notts).

home(yorkshire, headingley).
home(warwickshire, edgbaston).
home(notts, trent_bridge).

plays_at(X, Y) :-
  plays_for(X, Z),
  home(Z, Y).

% Where does bresnan play?
%| ?- plays_at(bresnan, Where).
% (one answer)

% Who plays at Headingley?
% | ?- plays_at(Who, headingley).
% (three answers)
