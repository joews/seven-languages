% a board has eight queens
eight_elements(List) :- length(List, 8).

% each column must be in the range 1-8
valid_queen((Row, Col)) :-
  Range = [1,2,3,4,5,6,7,8],
  member(Row, Range),
  member(Col, [1,2,3,4,5,6,7,8]).

valid_board([]).
valid_board([Head|Tail]) :-
  valid_queen(Head),
  valid_board(Tail).

% Unify Rows with the list of Row values in the input data
rows([], []).
rows([(Row, _)|QueenTail], [Row|RowsTail]) :-
  rows(QueenTail, RowsTail).

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
  eight_elements(Board),
  valid_board(Board),

  rows(Board, Rows),
  cols(Board, Cols),
  diagonal1(Board, Diagonal1),
  diagonal2(Board, Diagonal2),

  fd_all_different(Rows),
  fd_all_different(Cols),
  fd_all_different(Diagonal1),
  fd_all_different(Diagonal2).

% Too slow! too many permutations to consider to unify Board.
% eight_queens(Board).

% Provide some grounding by filling in the rows, but still too slow
% eight_queens([(1, A),(2, B),(3, C),(4, D),(5, E),(6, F),(7, G),(8, H)]);

