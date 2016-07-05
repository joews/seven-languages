% Each column must be a number 1-8 inclusive
valid_cols([]).
valid_cols([Head|Tail]) :-
  member(Head, [1,2,3,4,5,6,7,8]),
  valid_board(Tail).

% Uniquely number NW-SE amd SW-NE diagonals for each (Row, Col).
% Row is represented by the index of Col in Cols.
diagonal1([], []).
diagonal1([Col|QueenTail], [Diagonal|DiagonalTail]) :-
  length(QueenTail, TailLen),
  Row is 8 - TailLen,
  Diagonal is Col - Row,
  diagonal1(QueenTail, DiagonalTail).

diagonal2([], []).
diagonal2([Col|QueenTail], [Diagonal|DiagonalTail]) :-
  length(QueenTail, TailLen),
  Row is 8 - TailLen,
  Diagonal is Col + Row,
  diagonal2(QueenTail, DiagonalTail).

%
% eight queens rules
%
eight_queens(Cols) :-
  length(Cols, 8),
  valid_cols(Cols),

  % 8 unique rows are implicitly selected (represented by the indexes of Cols),
  %  and 8 unique cols are explicitly selected (the values of Cols)
  %  so we only need to work out a unique representation for each diagonal.
  diagonal1(Cols, Diagonal1),
  diagonal2(Cols, Diagonal2),

  fd_all_different(Cols),
  fd_all_different(Diagonal1),
  fd_all_different(Diagonal2).

% rows 1-8 must always be populated, so we can reduce the input
%  to a list of unique columns. This is tractible (unlike the first approach)
%  and has a neat API (unlike the second approach).
% My machine takes around one minute to find all of the solutions.

% eight_queens(Cols).
