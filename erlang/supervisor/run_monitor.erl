-module(run_monitor).
%
% Runner for toy monitor examples
% $ escript run_monitor.erl
%

main(_) ->
  % escript files don't auto compile referenced modules
  compile:file(monitor),
  compile:file(crasher),

  io:format("main~n"),

  % start children
  { ok, Monitor } = monitor:start(),
  { ok, Worker } = crasher:start(),

  % tell Monitor to watch Worker for crashes
  Monitor ! { monitor, Worker },

  % Send worker a normal message, then cause it to crash
  Worker ! ping,
  Worker ! crash,

  % Monitor reports Worker's crash.
  % The next step is a supervisor that can restart the child process itself.

  % Keep the app server alive so we see output from
  %  the Worker and Monitor processes
  loop().

loop() ->
  receive
    _ -> loop()
  end.
