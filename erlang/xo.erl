-module(xo).

% day2 bonus exercise: tic tac toe/noughts and crosses

% Expect a single argument containing the board pieces as a string,
% valid inputs are x (cross), o (nought) or _ (not yet played).
main([BoardStr|[]]) when length(BoardStr) == 9 ->
  log_board(BoardStr),
  Board = parse_input(BoardStr),
  Result = result(Board),
  io:format("The result is: ~p~n", [Result]);

main(_) ->
  io:format("Usage: escript xo.erl xoxx_o_xo").

% my first instinct was to write this like a prolog program.
% maybe there is a way to reduce the boilerplate,
%  though I like the raw readability.
% I guess this would get untenable if we had to validate
%  that the state of the board is legal, though.

% rows
result({ C, C, C, _, _, _, _, _ ,_ }) when (C == x) or (C == o) -> C;
result({ _, _, _, C, C, C, _, _ ,_ }) when (C == x) or (C == o) -> C;
result({ _, _, _, _, _, _, C, C, C }) when (C == x) or (C == o) -> C;

% cols
result({ C, _, _, C, _, _, C, _, _ }) when (C == x) or (C == o) -> C;
result({ _, C, _, _, C, _, _, C, _ }) when (C == x) or (C == o) -> C;
result({ _, _, C, _, _, C, _, _, C }) when (C == x) or (C == o) -> C;

% diagonals
result({ C, _, _, _, C, _, _, _, C }) when (C == x) or (C == o) -> C;
result({ _, _, C, _, C, _, C, _, _ }) when (C == x) or (C == o) -> C;

result(Board) ->
  % I can't define not_piece externally (bad function not_piece),
  %  but I can define it anonymously here and assign to a variable.
  % TODO why is this?
  NotPiece = fun(C) -> not(is_piece(C)) end,
  Incomplete = lists:any(NotPiece, tuple_to_list(Board)),
  if
    Incomplete -> no_winner;
    true -> cat
  end.

is_piece(C) -> (C == x) or (C == o).
not_piece(C) -> not is_piece(c).

% translate a string to tuple of atoms
% input : "abcd"
% output: { a, b, c, d }
parse_input(Str) ->
  Chars = re:split(Str, "", [{ return, list }]),
  Atoms = [list_to_atom(C) || C <- Chars, length(C) > 0],
  list_to_tuple(Atoms).

% impure: log board a 3x3 char matrix
log_board(Board) ->
  log_row(Board, 1),
  log_row(Board, 2),
  log_row(Board, 3).

log_row(Board, RowNum) ->
  io:format("~p~n", [string:substr(Board, RowNum, 3)]).
