%%% @author Andreas Pauley <>
%%% Created :  7 Feb 2013 by Andreas Pauley <>

-module(overlord).

-export([start/0,
         stop/0,
         restart/0,
         minion_info/0,
         sing/0]).

start() ->
  application:start(overlord).

stop() ->
  application:stop(overlord).

restart() ->
  stop(),
  start().

minion_info() ->
  hypnosponge:minion_info().

sing() ->
  hypnosponge:sing().
