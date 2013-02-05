%% The hypno-sponge is used for Minion Mind Control (MMC).

-module(sponge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {minions=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, start/0, stop/0, restart/0]).
-export([enslave/0, pids/0, process_info/0,
         minion_info/0, sing/0, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start() ->
  gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

restart() ->
  stop(),
  start().

stop() ->
  gen_server:call(?SERVER,stop,?DEFAULT_TIMEOUT).

enslave() ->
  call(enslave).

pids() ->
  call(minion_pids).

process_info() ->
  call(process_info).

minion_info() ->
  call(minion_info).

sing() ->
  call(sing).

send(Message) ->
  call({send, Message}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  darklord_utils:code_loads(),
  Pids = enslave_nodes(),
  State = #state{minions=Pids},
  {ok, State}.

handle_call(enslave, _From, State) ->
  Pids = enslave_nodes(),
  NewState = State#state{minions=Pids},
  {reply,{ok, Pids},NewState};

handle_call(minion_pids, _From, State) ->
  Pids = State#state.minions,
  {reply,{ok, Pids},State};

handle_call(process_info, _From, State) ->
  Pids = State#state.minions,
  Info = [erlang:process_info(Pid) || Pid <- Pids],
  {reply,{ok, Info},State};

handle_call(sing, _From, State) ->
  ok = minion_message(sing, State),
  {reply,ok,State};

handle_call(minion_info, _From, State) ->
  ok = minion_message(minion_info, State),
  {reply,ok,State};

handle_call({send, Message}, _From, State) ->
  ok = minion_message(Message, State),
  {reply,ok,State};

handle_call(stop, _From, State) ->
  io:format("[~p] stop ~p~n",[?MODULE, _From]),
  {stop,normal,State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  io:format("[~p] ~p ~p 'DOWN', Reason: ~p\n",[?MODULE, Pid, _Ref, _Reason]),
  Pids = State#state.minions,
  NewPids = Pids -- [Pid],
  NewState = #state{minions=NewPids},
  {noreply, NewState};
handle_info(_Info, State) ->
  io:format("[~p] handle_info ~p~n",[?MODULE, _Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("[~p] terminate ~p~n",[?MODULE, _Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

call(CallName) ->
  %% darklord_utils:code_loads(),
  gen_server:call(?SERVER,CallName,?DEFAULT_TIMEOUT).

enslave_nodes() ->
  enslave_nodes(nodes()).

enslave_nodes(Nodes) ->
  [enslave_node(Node) || Node <- Nodes].

enslave_node(Node) ->
  io:format("[~p] Enslaving Node ~p ... ",[?MODULE, Node]),
  Minion = spawn(Node, minion, aye_dark_overlord, [self()]),
  _Ref = erlang:monitor(process, Minion),
  io:format("~p~n",[Minion]),
  Minion.

minion_message(Message, State) ->
  MinionPids = State#state.minions,
  MessageFun = fun(Pid) ->
                   Pid ! Message
               end,
  lists:foreach(MessageFun, MinionPids).
