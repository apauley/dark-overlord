%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 18 Jan 2013 by Andreas Pauley <>

-module(darklord_utils).

-export([multicall/1, multicall/2,
         load_code/0, load_code/1]).

multicall(FunctionName) ->
  multicall(FunctionName, _Args=[]).

multicall(FunctionName, Args) ->
  multicall(FunctionName, Args, _Timeout=10000).

multicall(FunctionName, Args, Timeout) ->
  code_loads(),
  rpc:multicall(minion, FunctionName, Args, Timeout).

code_loads() ->
  abcast = c:nl(minion).

load_code() ->
  [load_code(Node) || Node <- nodes()].

load_code(Node) ->
  ModulesToLoad = [minion],
  [load_code(Module, Node) || Module <- ModulesToLoad].

load_code(Module, Node) ->
  {_Module, Binary, Filename} = code:get_object_code(Module),
  rpc:call(Node, code, load_binary, [Module, Filename, Binary]).
