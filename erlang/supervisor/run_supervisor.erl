-module(run_supervisor).
-export([main/0]).

%
% Runner for toy supervisor examples
% Run from REPL - escript can't find registered processes,
%
% 1> c(run_supervisor).
% 2> run_supervisor:main().
%
main() ->
  % REPL doesn't auto compile referenced modules
  compile:file(mini_supervisor),
  compile:file(crasher),

  % start the supervisor
  { ok, Supervisor } = mini_supervisor:start(),

  % tell Supervisor to start a new worker
  Supervisor ! new,

  % Yield this process for a short while to let the Supervisor
  %  finish spawning the child. The REPL does this for us by virtue
  %  of waiting for a slow human to input the next command!
  timer:sleep(10),

  % Send the registered worker a normal message, then cause it to crash
  crasher_service ! ping,
  crasher_service ! crash,

  % Monitor reports Worker's crash.
  % The next step is a supervisor that can restart the child process itself.
  % yield to Supervisor for a short while
  timer:sleep(10),

  % it's still alive!
  crasher_service ! ping.


