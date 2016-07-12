-module(crasher).
-export([start/0, loop/0]).

% A server process that crashes on demand
% For testing diy_supervisor.erl.

start() ->
  io:format("Starting crasher process~n"),
  { ok, spawn(fun crasher:loop/0) }.

loop() ->
  receive
    ping ->
      io:format("pong~n"),
      loop();
    crash ->
      io:format("crash~n"),
      exit({ request_crash }),
      loop()
  end.


