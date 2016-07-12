-module(mini_supervisor).
-export([start/0, loop/0]).

% A tiny process supervisor (`supervisor` isn't a valid module name)
% Implements the essential parts of the OTP supervisor behavior
%  (starting child processes and restarting on crash).

start() ->
  io:format("Starting supervisor~n"),
  { ok, spawn(fun mini_supervisor:loop/0) }.

loop() ->
  % register to receive process end events
  process_flag(trap_exit, true),

  receive
    new ->
      io:format("Starting a new child process with name crasher_service~n"),
      % spawn a new process linked to this process.

      % register the process by name so other processes can find it
      Worker = spawn_link(fun crasher:loop/0),
      register(crasher_service, Worker),
      loop();
    { 'EXIT', From, Reason } ->
      io:format("process ~p crashed because ~p. Restarting.~n", [From, Reason]),
      self() ! new,
      loop()
  end.
