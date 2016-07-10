-module(reply).
-export([loop/0]).
-export([send/2]).

% a minimal synchronous message service

% We can still send a message from the REPL. This time we need to tell
% the spawned process our own Pid (`self()`) so it knows where to send
%  the response message:

% 1> c(reply).
% 2> Proc = spawn(fun reply:loop/0).
% 3> Proc ! { self(), ping }.

% We don't receive a reply! We need to tell our own process to receive it:
% 4> Proc ! { self(), ping },
% 4> receive
% 4>   X -> X
% 4> end.

% our function send/2 handles the send/receive boilerplate:
% 5> reply:send(Proc, ping).
% pong

% server
loop() ->
  receive
    % Receive clauses match a Pid that can receive the reply
    { Client, ping } ->
      respond(Client, pong),
      loop();
    { Client, Other } ->
      respond(Client, unknown),
      loop();
    _ ->
      io:format("Error: No Pid~n")
  end.

respond(ClientPid, Response) ->
  % artificial latency of 0-1 seconds
  timer:sleep(trunc(random:uniform() * 1000)),
  ClientPid ! Response.

% client API
% To: a reply:loop process
% Input: the message to send
% returns: the reply:loop reply
send(To, Input) ->
  % self: the current process' pid
  To ! { self(), Input },
  receive
    Response -> Response
  end.
