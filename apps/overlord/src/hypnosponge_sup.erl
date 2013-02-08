%%%-------------------------------------------------------------------
%%% @author Andreas Pauley <>
%%%-------------------------------------------------------------------
-module(hypnosponge_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  RestartStrategy = one_for_all,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 60,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 5000,
  Type = worker,

  AChild = {hypnosponge, {hypnosponge, start_link, [self()]},
            Restart, Shutdown, Type, [hypnosponge]},

  log("Hello from the hypnosponge supervisor~n", []),
  {ok, {SupFlags, [AChild]}}.

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
