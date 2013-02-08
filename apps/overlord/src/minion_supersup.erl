%%%-------------------------------------------------------------------
%%% @author Andreas Pauley <>
%%%-------------------------------------------------------------------
-module(minion_supersup).

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
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 3,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 5000,
  Type = worker,

  AChild = {minion, {minion, start_link, [HypnoSpongePid]},
            Restart, Shutdown, Type, [minion]},
  log("The minion supervisor has been started on ~p with pid ~p~n",
      [node(), self()]),

  {ok, {SupFlags, [AChild]}}.

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
