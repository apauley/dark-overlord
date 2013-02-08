%%%-------------------------------------------------------------------
%%% @author Andreas Pauley <>
%%%-------------------------------------------------------------------
-module(minion_supersup).

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
  RestartStrategy = simple_one_for_one,
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 3,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  Restart = temporary,
  Shutdown = 5000,
  Type = worker,

  Child = {minion_makeshift_sup,
           {minion_makeshift_sup, start_link, []},
           Restart, Shutdown, Type, [minion_makeshift_sup]},
  log("The minion supervisor has been started on ~p with pid ~p~n",
      [node(), self()]),

  {ok, {SupFlags, [Child]}}.

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
