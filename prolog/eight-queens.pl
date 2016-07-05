% each column must be in the range 1-8
valid_queen((Row, Col)) :-
  member(Col, [1,2,3,4,5,6,7,8]).

valid_board([]).
valid_board([Head|Tail]) :-
  valid_queen(Head),
  valid_board(Tail).

% Unify Cols with the list of Col values in the input data
cols([], []).
cols([(_, Col)|QueenTail], [Col|ColsTail]) :-
  cols(QueenTail, ColsTail).

% Uniquely number NW-SE amd SW-NE diagonals for each (Row, Col).
diagonal1([], []).
diagonal1([(Row, Col)|QueenTail], [Diagonal|DiagonalTail]) :-
  Diagonal is Col - Row,
  diagonal1(QueenTail, DiagonalTail).

diagonal2([], []).
diagonal2([(Row, Col)|QueenTail], [Diagonal|DiagonalTail]) :-
  Diagonal is Col + Row,
  diagonal2(QueenTail, DiagonalTail).

%
% eight queens rules
%
eight_queens(Board) :-
  Board = [(1, _),(2, _),(3, _),(4, _),(5, _),(6, _),(7, _),(8, _)],
  valid_board(Board),

  cols(Board, Cols),
  diagonal1(Board, Diagonal1),
  diagonal2(Board, Diagonal2),

  fd_all_different(Cols),
  fd_all_different(Diagonal1),
  fd_all_different(Diagonal2).

% optimize by removing row unification; rows 1-8 must necessarily
%  be populated so we can set that as an explicit input constraint.
% the problem is now tractible!
% eight_queens([(1, A),(2, B),(3, C),(4, D),(5, E),(6, F),(7, G),(8, H)]);
