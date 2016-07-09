-module(day1).
-export([pick/2]).
-export([total_price/1]).

main(_) ->
  log("Testing day 2 exercises"),

  % test total_price
  CheeseCart = [{ appenzeller, 0.4, 10 }, { manchego, 0.3, 12.80 }, { cambozola, 0.5, 15.10 }],
  log(total_price(CheeseCart)),

  % test pick
  CheeseDB = [{ brie, creamy }, { gouda, nutty }, { lancashire, tangy }],
  log(pick(CheeseDB, brie)),
  log(pick(CheeseDB, roquefort)),

  log(pick_(CheeseDB, gouda)),
  log(pick_(CheeseDB, camembert)),

  log("Done").

% pick
% List: [{ key1, value1 }, { key2, value2 }, ...]
% Key: a key from a List tuple.
% return the value associated with Key from List.

% diy
% TODO neater way to write this comprehension?
pick(List, Key) ->
  case [V || { K, V } <- List, K == Key] of
    [Value] -> Value;
    _ -> 404
  end.

% using built-in keyfind
pick_(List, Key) ->
  case lists:keyfind(Key, 1, List) of
    { _, Value } -> Value;
    _ -> 404
  end.

% Cart: [{ item, quantity, price }].
% return [{ item, total_price }]
total_price(Cart) ->
  [{ Item, Quantity * Price } || { Item, Quantity, Price } <- Cart].


log (Msg) ->
  io:format("~p~n", [Msg]).
