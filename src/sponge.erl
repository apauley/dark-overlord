%% The hypno-sponge is used for Minion Mind Control (MMC).

-module(sponge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

%% -record(state, {pids=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start/0, stop/0]).
-export([enslave/0, sing/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
  start_link().

stop() ->
  gen_server:call(?SERVER,stop,?DEFAULT_TIMEOUT).

enslave() ->
  gen_server:call(?SERVER,enslave,?DEFAULT_TIMEOUT).

sing() ->
  multicall(sing).
  %% gen_server:call(?SERVER,sing,?DEFAULT_TIMEOUT).

%% ------------------------------------------------------------------
%% Internal Startup Functions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  Pids = enslave_nodes(nodes()),
  {ok, Pids}.

handle_call(enslave, _From, State) ->
  Pids = enslave_nodes(),
  {reply,{ok, Pids},State};

handle_call(stop, _From, State) ->
  {stop,normal,State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

enslave_nodes() ->
  enslave_nodes(nodes()).

enslave_nodes(Nodes) ->
  code_loads(),
  [enslave_node(Node) || Node <- Nodes].

enslave_node(Node) ->
  io:format("[~p] Enslaving Node ~p ... ",[?MODULE, Node]),
  Minion = spawn(Node, minion, aye_dark_overlord, [self()]),
  io:format("~p~n",[Minion]),
  Minion.

multicall(FunctionName) ->
  multicall(FunctionName, _Args=[]).

multicall(FunctionName, Args) ->
  code_loads(),
  rpc:multicall(minion, FunctionName, Args).

code_loads() ->
  c:nl(minion).
