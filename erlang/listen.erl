% at first I called this module "receive".
% That is not allowed because `receieve` is a keyword.
-module(listen).
-export([loop/0]).

% a minimal receive-only process
% Call from the REPL:
% 1> c(receive).
% 2> Proc = spawn(fun listen:loop/0).
% 3> Proc ! "foo".

% Proc is a PID (process ID).
% Process IDs can be assigned a name, to make them easier to find:
% 4> register(ping, Proc).
% 5> ping ! pong.

% There are many ways to spawn - http://erlang.org/doc/man/erlang.html#spawn-1
% To start a process on another machine (assigned to Node):
% spawn(Node, fun translate:loop/0)

% To spawn on another Node:
% Start two Erlang sessions with short names (-sname):
% $ erl -sname ann
% $ erl -sname bob

% Each session has a REPL. In the "ann" repl:
% (ann@JoeMBP2)1> c("listen").
% (ann@JoeMBP2)2> Bob = spawn(bob@JoeMBP2, fun listen:loop/0).
% (ann@JoeMBP2)3> Bob ! ping.

% To see the current node name:
% (ann@JoeMBP2)5> node().
% ann@JoeMBP2

% To see which nodes we are connected to:
% (ann@JoeMBP2)6> nodes().
% [bob@JoeMBP2]
% (bob@JoeMBP2)4> nodes().
% [ann@JoeMBP2]

% recursive listen loop
loop() ->
  receive
    ping ->
      io:format("pong~n"),
      loop();
    foo ->
      io:format("bar~n"),
      loop();
    Other ->
      io:format("I don't know [~p]~n", [Other]),
      loop()
  end.

