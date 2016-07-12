-module(monitor).
-export([start/0, loop/0]).

% A process that can monitor other processes and be notified
%  if they crash. The first step towards a supervisor.
start() ->
  io:format("Starting monitor~n"),
  { ok, spawn(fun monitor:loop/0) }.

loop() ->
  % register to receive process end events
  process_flag(trap_exit, true),

  receive
    { monitor, Process } ->
      io:format("Monitoring process ~p~n", [Process]),
      link(Process),
      loop();
    { 'EXIT', From, Reason } ->
      io:format("process ~p crashed because ~p~n", [From, Reason]),
      loop()
  end.
