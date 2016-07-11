-module(server_supervisor).
-behavior(supervisor).

%
% OTP app root supervisor
% Controlled by app.erl
% Supervises (starts, stops, restarts) server.erl.
%

% TODO
% Start several processes with different configuration
% (e.g. write to differnet files)
-export([start_link/0, init/1]).


%
% supervisor callbacks
%

% External API.
% supervisor:start_link spawns a new supervisor process and
%  calls init with the given Args.
start_link() ->
  Args = [],
  supervisor:start_link(?MODULE, Args).

% Args: passed from start_link's supervisor_start:link invocation
% returns { ok, { SupervisorFlags, ChildSpecs } }
% - supervisor flags define how, how often and when crashed child processes should be restarted
% - child spects describe the child processes that this supervisor looks after
init(_Args) ->
  io:format("Starting server_supervisor~n"),

  SupFlags = #{
    strategy => one_for_one,  % if a child crashes, restart only that child
    intensity => 3,           % if more than 3 restarts happen in a 5 second
    period => 5               %  period, terminate all children and then self.
  },

  ChildSpec = #{
    id => server,                   % an internal id for the supervisor
    start => { server, start, [] }, % MFA (module, function, arguments): what to start
    restart => permanent,           % always restart this process if it crashes
    shutdown => 2,                  % all children 2 seconds to gracefully shut down before killing
    type => worker                 % supervisor|worker (default worker)
  },

  { ok, { SupFlags, [ChildSpec] } }.
