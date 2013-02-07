%%%-------------------------------------------------------------------
%%% @author Andreas Pauley <>
%%%-------------------------------------------------------------------
-module(minion_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(HypnoSpongePid) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [HypnoSpongePid]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([HypnoSpongePid]) ->
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 3,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = permanent,
  Shutdown = 5000,
  Type = worker,

  AChild = {minion, {minion, start_link, [HypnoSpongePid]},
            Restart, Shutdown, Type, [minion]},

  {ok, {SupFlags, [AChild]}}.
