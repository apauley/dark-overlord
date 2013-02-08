%% The hypnosponge is used for Minion Mind Control (MMC).

-module(hypnosponge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {minion_supervisor,
                minions=[]}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/0]).

-export([pids/0,
         process_info/0,
         minion_info/0,
         minion_crash/0,
         minion_exit/1,
         sing/0,
         send/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(SpongeSupervisorPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [SpongeSupervisorPid], _Options=[]).

stop() ->
  gen_server:call(?SERVER,stop,?DEFAULT_TIMEOUT).

pids() ->
  gen_server:call(?SERVER,minion_pids,?DEFAULT_TIMEOUT).

process_info() ->
  gen_server:call(?SERVER,process_info,?DEFAULT_TIMEOUT).

minion_info() ->
  gen_server:call(?SERVER,minion_info,?DEFAULT_TIMEOUT).

minion_crash() ->
  gen_server:call(?SERVER,minion_crash,?DEFAULT_TIMEOUT).

minion_exit(Reason) ->
  gen_server:call(?SERVER,{minion_exit, Reason},?DEFAULT_TIMEOUT).

sing() ->
  gen_server:call(?SERVER,sing,?DEFAULT_TIMEOUT).

send(Message) ->
  gen_server:call(?SERVER,{send, Message},?DEFAULT_TIMEOUT).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([SpongeSupervisorPid]) ->
  self() ! {start_minion_supervisor, SpongeSupervisorPid},
  log("The hypnosponge has been started on ~p with pid ~p~n",[node(), self()]),
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

handle_call(minion_crash, _From, State) ->
  ok = minion_message(crash_you_worthless_fool, State),
  {reply,havoc_caused,State};

handle_call({minion_exit, Reason}, _From, State) ->
  ok = minion_message({exit, Reason}, State),
  {reply,exit_command_givenState};

handle_call({send, Message}, _From, State) ->
  ok = minion_message(Message, State),
  {reply,ok,State};

handle_call(stop, _From, State) ->
  log("stop ~p~n",[_From]),
  {stop,normal,State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({start_minion_supervisor, SpongeSupervisorPid}, State = #state{}) ->
  MinionSupSpec = {minion_sup,
                   {minion_sup, start_link, [self()]},
                   permanent,
                   10000,
                   supervisor,
                   [minion_sup]},
  {ok, MinionSupPid} = supervisor:start_child(SpongeSupervisorPid, MinionSupSpec),
  log("The minion supervisor has been started on ~p with pid ~p (attached to sponge supervisor ~p)~n",
      [node(), MinionSupPid, SpongeSupervisorPid]),
  log("hypnosponge_sup (~p) now has these children: ~p~n",
      [SpongeSupervisorPid, supervisor:which_children(SpongeSupervisorPid)]),
  link(MinionSupPid),

  NewState = enslave_nodes(State#state{minion_supervisor=MinionSupPid}),
  {noreply, NewState};
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  log("~p ~p 'DOWN', Reason: ~p\n",[Pid, _Ref, _Reason]),
  Pids = State#state.minions,
  NewPids = Pids -- [Pid],
  NewState = #state{minions=NewPids},
  {noreply, NewState};
handle_info(_Info, State) ->
  log("Unexpected handle_info message: ~p~n",[_Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  log("terminate Reason: ~p~n",[_Reason]),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

enslave_nodes(State = #state{minion_supervisor=MinionSupPid}) ->
  Minions = enslave_nodes(MinionSupPid),
  NewState = State#state{minions=Minions},
  NewState;
enslave_nodes(MinionSupPid) when is_pid(MinionSupPid) ->
  enslave_nodes(nodes(), MinionSupPid).

enslave_nodes(Nodes, MinionSupPid) ->
  [enslave_node(Node, MinionSupPid) || Node <- Nodes].

enslave_node(Node, MinionSupPid) ->
  log("Enslaving Node ~p ...~n",[Node]),
  darklord_utils:load_code(Node),
  {ok, Minion} = supervisor:start_child(MinionSupPid, [Node]),
  _Ref = erlang:monitor(process, Minion),
  log("Enslaved ~p on node ~p~n",[Minion, Node]),
  log("minion_sup (~p) now has these children: ~p~n",
      [MinionSupPid, supervisor:which_children(MinionSupPid)]),
  Minion.

minion_message(Message, State) ->
  MinionPids = State#state.minions,
  MessageFun = fun(Pid) ->
                   Pid ! Message
               end,
  lists:foreach(MessageFun, MinionPids).

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
