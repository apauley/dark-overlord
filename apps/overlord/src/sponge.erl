%% The hypno-sponge is used for Minion Mind Control (MMC).

-module(sponge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {minion_supervisor,
                minions=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/0]).
-export([pids/0, process_info/0,
         minion_info/0, sing/0, send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(OverlordSupervisorPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [OverlordSupervisorPid], _Options=[]).

stop() ->
  gen_server:call(?SERVER,stop,?DEFAULT_TIMEOUT).

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

init([OverlordSupervisorPid]) ->
  darklord_utils:code_loads(),
  self() ! {start_minion_supervisor, OverlordSupervisorPid},
  {ok, #state{}}.

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

handle_info({start_minion_supervisor, OverlordSupervisorPid}, State = #state{}) ->
  MinionSupSpec = {minion_sup,
                   {minion_sup, start_link, [self()]},
                   temporary,
                   10000,
                   supervisor,
                   [minion_sup]},
  {ok, MinionSupPid} = supervisor:start_child(OverlordSupervisorPid, MinionSupSpec),
  link(MinionSupPid),

  NewState = enslave_nodes(State#state{minion_supervisor=MinionSupPid}),
  {noreply, NewState};
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

enslave_nodes(State = #state{minion_supervisor=MinionSupPid}) ->
  Minions = enslave_nodes(MinionSupPid),
  NewState = State#state{minions=Minions},
  NewState;
enslave_nodes(MinionSupPid) when is_pid(MinionSupPid) ->
  enslave_nodes(nodes(), MinionSupPid).

enslave_nodes(Nodes, MinionSupPid) ->
  [enslave_node(Node, MinionSupPid) || Node <- Nodes].

enslave_node(Node, MinionSupPid) ->
  io:format("[~p] Enslaving Node ~p ...~n",[?MODULE, Node]),
  StartChildReturn = supervisor:start_child(MinionSupPid, [Node]),
  Minion = case StartChildReturn of
             {ok, Pid} when is_pid(Pid) ->
               Pid;
             {error, Pid} when is_pid(Pid)  ->
               io:format("[~p] start_child returned error after spawning successfully. WHY??????~n",[?MODULE]),
               Pid
           end,
  _Ref = erlang:monitor(process, Minion),
  io:format("Enslaved ~p on node ~p~n",[Minion, Node]),
  Minion.

minion_message(Message, State) ->
  MinionPids = State#state.minions,
  MessageFun = fun(Pid) ->
                   Pid ! Message
               end,
  lists:foreach(MessageFun, MinionPids).
