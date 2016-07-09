% run with escript:
% escript lists.erl
-module(lists).
-export([main/1]).

%
% exploring higher-order functions and lists
%
main (_) ->
  L = [1, 2, 3, 4, 5],

  % The built-in `lists` module has higher-order list functions
  Plus1 = fun(X) -> X + 1 end,
  log(lists:map(Plus1, L)),

  Evens = lists:filter(fun(N) -> N rem 2 == 0 end, L),
  log(Evens),

  % the usual functions are there: all, any, take, dropwhile, foldl, etc...

  % no automatic currying:
  % AllPlus1 = lists:map(Plus1), % undefined function lists:map/1

  % use a wrapper function instead:
  AllPlus1 = fun(List) -> lists:map(Plus1, List) end,
  log(AllPlus1(L)),

  % list comprehensions

  % Map only
  TwoN = [trunc(math:pow(2, N)) || N <- L],
  log(TwoN),

  % Map and filter clauses
  % List comprehensions have the form:
  % [Expression || Clause1, Clause2, ..., ClauseN]
  % Clauses can provide values, e.g. N <- TwoN,
  %  or filter values, e.g. N > 4.
  log([N + 1 || N <- TwoN, N > 4, N < 32]),

  % Pattern matching in list comprehensions
  People = [{ joe, 30 }, { amy, 29 }, { peach, 4}, { plum, 1 }],
  log([Name || { Name, Age } <- People, Age < 10]),

  % List comprehensions can have several inputs. Erlang yields
  %  cartesian product pairs to the map expression.
  In1 = [a, b],
  In2 = [x, y, z],

  % [{a,x},{a,y},{a,z},{b,x},{b,y},{b,z}]
  log([{ A, B } || A <- In1, B <- In2 ]),

  test_list_functions().


% implementing some list functions
map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

filter(_, []) -> [];
filter(F, [H|T]) ->
  case F(H) of
    true -> [H | filter(F, T)];
    _    -> filter(F, T)
  end.

any(_, []) -> false;
any(F, [H|T]) ->
  case F(H) of
    true -> true;
    false -> any(F, T)
  end.

all(_, []) -> false;
all(F, [H|T]) ->
  case F(H) of
    true -> all(F, T);
    false -> false
  end.

foldl(_, Prev, []) -> Prev;
foldl(F, Prev, [H|T]) ->
  foldl(F, F(H, Prev), T).

take(_, 0) -> [];
take([H|T], N) when N > 0 ->
  [H|take(T, N - 1)].

% TODO not tail recursive
% TODO implement append
reverse([]) -> [];
reverse([H|T]) ->
  lists:append(reverse(T), [H]).

% test home made list functions
% TODO find a unit test library
test_list_functions() ->
  Input = [1, 1, 2, 3, 5, 8, 13],

  Double = fun(N) -> N + N end,
  Small = fun(N) -> N < 6 end,
  Sum = fun(X, Y) -> X + Y end,

  io:format("~nTesting list functions:~n"),

  log([map,
      map(Double, Input) == lists:map(Double, Input),
      map(Double, Input)
  ]),

  log([filter,
      filter(Small, Input) == lists:filter(Small, Input),
      filter(Small, Input)
  ]),

  log([any,
      any(Small, Input) == lists:any(Small, Input),
      any(Small, Input)
  ]),

  log([all,
      all(Small, Input) == lists:all(Small, Input),
      all(Small, Input)
  ]),

  log([foldl,
      foldl(Sum, 0, Input) == lists:foldl(Sum, 0, Input),
      foldl(Sum, 0, Input)
  ]),

  % there isn't a lists:take
  log(take(Input, 4)),

  log([reverse,
      reverse(Input) == lists:reverse(Input),
      reverse(Input)
  ]),

  io:format("Done testing list functions~n").

log (Msg) ->
  io:format("~p~n", [Msg]).
