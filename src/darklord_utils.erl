%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 18 Jan 2013 by Andreas Pauley <>

-module(darklord_utils).

-export([multicall/1, multicall/2, code_loads/0]).

multicall(FunctionName) ->
  multicall(FunctionName, _Args=[]).

multicall(FunctionName, Args) ->
  multicall(FunctionName, Args, _Timeout=10000).

multicall(FunctionName, Args, Timeout) ->
  code_loads(),
  rpc:multicall(minion, FunctionName, Args, Timeout).

code_loads() ->
  {ok, minion} = c:c(minion),
  abcast = c:nl(minion).
