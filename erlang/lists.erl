% run with escript:
% escript lists.erl
-module(lists).
-export([main/1]).

%
% exploring higher-order functions and lists
%
main (Args) ->
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

  test(),
  log(done).



% implementing some list functions
map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].

filter(_, []) -> [];
filter(F, [H|T]) ->
  case F(H) of
    true -> [H | filter(F, T)];
    _    -> filter(F, T)
  end.

any(F, []) -> false;
any(F, [H|T]) ->
  case F(H) of
    true -> true;
    false -> any(F, T)
  end.

all(F, []) -> false;
all(F, [H|T]) ->
  case F(H) of
    true -> all(F, T);
    false -> false
  end.

foldl(F, Prev, []) -> Prev;
foldl(F, Prev, [H|T]) ->
  foldl(F, F(H, Prev), T).

take(_, 0) -> [];
take([H|T], N) when N > 0 ->
  [H|take(T, N - 1)].


% test home made list functions
% TODO find a unit test library
test() ->
  Input = [1, 1, 2, 3, 5, 8, 13],

  Double = fun(N) -> N + N end,
  Small = fun(N) -> N < 6 end,
  Sum = fun(X, Y) -> X + Y end,

  log(testing),

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

  log(tests_done).

log (Msg) ->
  io:format("~p~n", [Msg]).
