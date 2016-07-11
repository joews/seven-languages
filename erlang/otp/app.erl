-module(app).
-behavior(application).
-export([start/2, stop/1]).

%
% OTP application:
% Starts and stops the root supervisor. Cleans up after stopping.
% Configured by app.app.
% http://erlang.org/doc/design_principles/applications.html
%
% Start the repl with logging:
% $ erl -boot start_sasl
%
% Control from the repl:
% 1> application:start(app).
% 2> application:stop(app).
%
% See what applications are loaded and running:
% 3> application:loaded_applications().
% 4> application:which_applications().
%

% start: called by the application start routine, which gets arguments
%  from the manifest app.app.
% To start from the REPL:
%
% 1> application:start(app).

% StartType, StartArgs -> { ok, Pid [, State] }
start(Type, Args) ->
  % io:format("arf"),
  io:format("Starting app [~p] [~p] ~n", [Type, Args]),
  % { ok, self(), test_state }.

  { ok, Pid } = server_supervisor:start_link(),
  State = {},

  { ok, Pid, State }.


% stop: called by the application stop routine after the app has
%  been stopped.
% To stop the app from the CLI:
% 1> application:stop(app).
stop(State) ->
  io:format("Cleaning up after app stopped [~p] ~n", [State]).
