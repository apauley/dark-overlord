%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 18 Jan 2013 by Andreas Pauley <>

-module(darklord_utils).

-export([multicall/1, multicall/2, code_loads/0]).

multicall(FunctionName) ->
  multicall(FunctionName, _Args=[]).

multicall(FunctionName, Args) ->
  code_loads(),
  rpc:multicall(minion, FunctionName, Args).

code_loads() ->
  c:nl(minion).
