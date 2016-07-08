% run with escript:
% escript flow_control.erl
-module(flow_control).
-export([main/1]).

main (Args) ->
  log(case1(aye)),
  log(case1(wat)),

  log(getUserName({ user, { name, "Joe" }})),
  log(getUserName({ droid, { name, "8t88" }})),

  log(getAnyName({ user, { name, "Joe" }})),
  log(getAnyName({ droid, { name, "8t88" }})),
  log(getAnyName({ droid, { name, "R2D2" },  {type, "R2" }})), % no match, because the extra field doesn't match a pattern

  log(if1(2)),
  log(if1(0)),

  log(done).



log (Msg) ->
  io:format("~p~n", [Msg]).

% case expression with pattern matching
% matches everything.
case1 (X) ->
  case X of
    aye -> yes;
    nay -> no;
    _ -> pardon
  end.

% tuple destructuring is common in Erlang
% an atom as the first element often acts as a "tag"
% sequential { atom, Value } pairs in a tuple can work as key-value pairs with
%  destructuring.
getUserName (Record) ->
  case Record of
    { user, { name, Name } } -> Name;
    _ -> not_a_user
  end.

% _ matches anything here, too, so we can use a kind of duck typing.
% the pattern must match the complete record.
% TODO is true duck typing possble - can we many -anything- with a { name, Name }
%  tuple?
getAnyName (Record) ->
  case Record of
    { _, { name, Name } } -> Name;
    _ -> anon
  end.

% if expression with guards
% every input must match one branch because the if expression must return
%  a value. If this is not possible the compiler throws an error:
%  > exception error: no true branch found when evaluating an if expression
% In this case the last branch could use `X = 0` or an always-true expression.
if1 (X) ->
  if
    X > 0 ->
      positive;
    X < 0 ->
      negative;
    % X = 0 ->   % also valid
      % zero
    % an always-true branch acts as "else".
    true ->
      zero
end.
