-module(overlord_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% start/0 is not really needed, just added it to play with manual starting.
-export([start/0]).

start() ->
  start(_StartType=normal, _StartArgs=[]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  overlord_sup:start_link().

stop(_State) ->
  ok.
