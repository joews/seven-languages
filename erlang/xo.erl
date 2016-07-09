-module(xo).

% day2 bonus exercise: tic tac toe/noughts and crosses

% Expect a single argument containing the board pieces as a string,
% valid inputs are x (cross), o (nought) or _ (not yet played).
main([Board|[]]) when length(Board) == 9 ->
  % log_board(Board),

  % TODO actually use the input parameter! need to convert the string
  % (actually an array of integers) into an array of atoms.
  % use this test input for now
  % use a for "available"
  B = [x, o, a,
       o, x, z,
       x, o, x],

  Result = result(B),

  io:format("The result is~n"),
  io:format(Result);

main(_) ->
  io:format("Usage: escript xo.erl xoxx_o_xo").

% my first instinct was to write this like a prolog program.
% I think that was not the best approach.
% TODO try another way.

% rows
result([C|[C|[C|[_|[_|[_|[_|[_|[_]]]]]]]]]) when (C == x) or (C == o) -> C;
result([_|[_|[_|[C|[C|[C|[_|[_|[_]]]]]]]]]) when (C == x) or (C == o) -> C;
result([_|[_|[_|[_|[_|[_|[C|[C|[C]]]]]]]]]) when (C == x) or (C == o) -> C;

% cols
result([C|[_|[_|[C|[_|[_|[C|[_|[_]]]]]]]]]) when (C == x) or (C == o) -> C;
result([_|[C|[_|[_|[C|[_|[_|[C|[_]]]]]]]]]) when (C == x) or (C == o) -> C;
result([_|[_|[C|[_|[_|[C|[_|[_|[C]]]]]]]]]) when (C == x) or (C == o) -> C;

% diagonals
result([C|[_|[_|[_|[C|[_|[_|[_|[C]]]]]]]]]) when (C == x) or (C == o) -> C;
result([_|[_|[C|[_|[C|[_|[C|[_|[_]]]]]]]]]) when (C == x) or (C == o) -> C;


result(Board) ->
  Incomplete = lists:any(fun(C) -> C == a end, Board),
  Draw = lists:all(fun(C) -> (C == x) or (C == o) end, Board),
  if
    Draw -> draw;
    Incomplete -> no_winner;
    true -> invalid_board
  end.

% impure: log board a 3x3 char matrix
log_board(Board) ->
  log_row(Board, 1),
  log_row(Board, 2),
  log_row(Board, 3).

log_row(Board, RowNum) ->
  io:format("~p~n", [string:substr(Board, RowNum, 3)]).
