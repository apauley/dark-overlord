-module(overlord_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start({takeover, OtherNode}, _StartArgs) ->
  darklord_utils:log(?MODULE, "Taking over from ~p~n", [OtherNode]),
  hypnosponge_sup:start_link();
start(_StartType, _StartArgs) ->
  darklord_utils:log(?MODULE, "Starting app: ~p~n", [_StartType]),
  hypnosponge_sup:start_link().

stop(_State) ->
  ok.
