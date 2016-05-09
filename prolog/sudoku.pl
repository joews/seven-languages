% Solve 4x4 Sudoku puzzles

% Puzzle: List of length 16
sudoku4(Puzzle, Solution) :-

  % The solution can be unified with the puzzle
  Solution = Puzzle,

  % Each element of the solution is a number in the range [1, 4].  
  fd_domain(Puzzle, 1, 4),

  % The puzzle can be unfied with a list of 16 variables...
  Puzzle = [
    S11, S12, S13, S14,
    S21, S22, S23, S24,
    S31, S32, S33, S34,
    S41, S42, S43, S44
  ],

  % ... which form four rows, four columns and four squares
  Row1 = [S11, S12, S13, S14],
  Row2 = [S21, S22, S23, S24],
  Row3 = [S31, S32, S33, S34],
  Row4 = [S41, S42, S43, S44],
  Col1 = [S11, S21, S31, S41],
  Col2 = [S12, S22, S32, S42],
  Col3 = [S13, S23, S33, S43],
  Col4 = [S14, S24, S34, S44],
  Squ1 = [S11, S12, S21, S22],
  Squ2 = [S13, S14, S23, S24],
  Squ3 = [S31, S32, S41, S42],
  Squ4 = [S33, S34, S43, S44],

  % Each row, col and square contains no repeated numbers 
  no_duplicates([
    Row1, Row2, Row3, Row4,
    Col1, Col2, Col3, Col4,
    Squ1, Squ2, Squ3, Squ4
  ]).

% Predicate: assert that each element of the given List
%  is a list that contains no repeated elements
no_duplicates([]).
no_duplicates([H|T]):-
  fd_all_different(H),
  no_duplicates(T).

% Example inputs
% sudoku4([
%   3, _, _, 2,
%   _, _, 1, _,
%   _, 3, _, _,
%   4, _, _, 1], 
% Solution).

% sudoku4([
%   _, _, 2, 3,
%   _, _, _, _,
%   _, _, _, _,
%   3, 4, _, _], 
% Solution).
