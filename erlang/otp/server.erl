-module(server).
-behavior(gen_server).

%
% OTP example server
% Run by server_supervisor
% May also be controlled standalone with start/0, stop/0.
%

% To start without a supervisor:
% 1> server:start().
%
% Send test messages:
% 2> gen_server:cast(my_name, test).
% 3> gen_server:call(my_name, test).
%

% TODO actually do something! like logging messages to a file
%  and searching logged messages

% used gen_server callbacks
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

% unused gen_server callbacks
-export([code_change/3, handle_info/2]).

% testing api (to start/stop without a supervisor)
-export([start/0, stop/0]).

% client api
-export([crash_client/0, crash_server/0]).

%
% gen_server lifecycle callbacks
%

% returns { ok, InitialState }
init(_Args) ->
  { ok, {} }.

terminate(Reason, _State) ->
  io:format("terminated with reason [~p]~n", [Reason]),
  ok.

%
% gen_server request callbacks
%

% usually servers will pattern match against request.
% here we have fall-through handle_call and handle_cast
%  functions for testing.

% call: a synchronous request that expects a response
% cast: an async request that does not expect a response.

% handle a "stop" request
% (usually sent by the stop() function to stop the app in
%  standalone mode, without a supervisor).
handle_cast(stop, State) ->
  io:format("received request to stop~n"),
  { stop, normal, State };

% handle a "crash" request by intentionally crashing
handle_cast(crash, State) ->
  io:format("received request to crash~n"),
  exit(server_crash),
  { stop, normal, State };

% default cast
% return { nreply, State' }
handle_cast(Request, State) ->
  io:format("handle unknown cast [~p]~n", [Request]),
  { noreply, State }.

% default call
% return {reply, Reply, State'}
handle_call(Request, _From, State) ->
  io:format("handle unknown call [~p}~n", [Request]),
  { reply, todo, State }.

%
% standalone operation (to run without a supervisor)
%
start() ->
  gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

stop() ->
  cast(stop).

%
% client api
%

% send a message to the server causing it to crash
%  (and the supervisor to restart, if running supervised).
% send this many times in a short space of time to break the
%  supervisor's restart intensity rules and cause the app to stop.
crash_server() ->
  cast(crash).

% cause the client process to crash
% if this is the repl, the process is auto-restarted by `erl`, I think.
% this doesn't affect the running application.
crash_client() ->
  exit(crash_client).

%
% helpers
%
cast(Request) ->
  gen_server:cast(?MODULE, Request).

%
% unused gen_server callbacks
%

% returns { ok, NewState }
code_change(_OldVersion, State, _Extra) ->
  io:format("Unhandled code change~n"),
  { ok, State}.

% receive non-request messages including exit messages
% if we send a message direct with serverProc ! message,
%  it is received here.
handle_info(Info, State) ->
  io:format("Unhandled info message [~p]~n", [Info]),
  { noreply, State }.
