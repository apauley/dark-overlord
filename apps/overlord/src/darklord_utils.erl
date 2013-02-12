%%% @author Andreas Pauley <>
%%% @copyright (C) 2013, Andreas Pauley
%%% Created : 18 Jan 2013 by Andreas Pauley <>

-module(darklord_utils).

-export([multicall/1, multicall/2,
         load_code/0, load_code/1,
         log/3, log_supervisor_children/3]).

multicall(FunctionName) ->
  multicall(FunctionName, _Args=[]).

multicall(FunctionName, Args) ->
  multicall(FunctionName, Args, _Timeout=10000).

multicall(FunctionName, Args, Timeout) ->
  rpc:multicall(minion, FunctionName, Args, Timeout).

load_code() ->
  [load_code(Node) || Node <- nodes()].

load_code(Node) ->
  ModulesToLoad = [?MODULE, minion_makeshift_sup, minion, sudoku],
  [load_code(Module, Node) || Module <- ModulesToLoad].

load_code(Module, Node) ->
  {_Module, Binary, Filename} = code:get_object_code(Module),
  rpc:call(Node, code, load_binary, [Module, Filename, Binary]).

log(Module, String, Params) ->
  Node = atom_to_list(node()),
  log("~s ~s [~p] ~p || "++String, [timestamp(), Node, Module, self()|Params]).

log(String, Params) ->
  Text = lists:flatten(io_lib:format(String, Params)),
  erlang:display_string(Text).

log_supervisor_children(Module, SupPid, SupName) ->
  Children = [Pid || {_,Pid,_WorkerOrSupervisor,_Name} <- supervisor:which_children(SupPid)],
  log(Module, "~p (~p) now has ~p children: ~p~n",
      [SupName, SupPid, length(Children), Children]).

timestamp() ->
  timestamp(os:timestamp()).

timestamp(Now) ->
  {_,{HH,NN,SS}} = calendar:now_to_local_time(Now),
  {_,_,XX} = Now,
  lists:flatten(io_lib:format("~2..0w:~2..0w:~2..0w.~3..0s", [HH,NN,SS,integer_to_list(XX)])).
