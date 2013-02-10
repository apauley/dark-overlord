%% The hypnosponge is used for Minion Mind Control (MMC).

-module(hypnosponge).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {minion_supervisor,
                minions=[],
                sudoku_started=false,
                sudoku_stats=dict:new()}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, stop/0]).

-export([pids/0,
         process_info/0,
         minion_nodes/0,
         minion_info/0,
         minion_crash/0,
         minion_exit/1,
         sponge_crash/0,
         sponge_exit/1,
         sing/0,
         sudoku_start/0,
         sudoku_stop/0,
         sudoku_stats/0,
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

minion_nodes() ->
  gen_server:call(?SERVER,minion_nodes,?DEFAULT_TIMEOUT).

minion_info() ->
  gen_server:call(?SERVER,minion_info,?DEFAULT_TIMEOUT).

minion_crash() ->
  gen_server:call(?SERVER,minion_crash,?DEFAULT_TIMEOUT).

minion_exit(Reason) ->
  gen_server:call(?SERVER,{minion_exit, Reason},?DEFAULT_TIMEOUT).

sponge_crash() ->
  gen_server:call(?SERVER,sponge_crash,?DEFAULT_TIMEOUT).

sponge_exit(Reason) ->
  gen_server:call(?SERVER,{sponge_exit, Reason},?DEFAULT_TIMEOUT).

sing() ->
  gen_server:call(?SERVER,sing,?DEFAULT_TIMEOUT).

sudoku_start() ->
  gen_server:call(?SERVER,sudoku_start,?DEFAULT_TIMEOUT).

sudoku_stop() ->
  gen_server:call(?SERVER,sudoku_stop,?DEFAULT_TIMEOUT).

sudoku_stats() ->
  gen_server:call(?SERVER,sudoku_stats,?DEFAULT_TIMEOUT).

send(Message) ->
  gen_server:call(?SERVER,{send, Message},?DEFAULT_TIMEOUT).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([SpongeSupervisorPid]) ->
  self() ! {start_minion_supervisor, SpongeSupervisorPid},
  log("Hello from the hypnosponge itself!~n",[]),
  {ok, #state{}}.

handle_call(minion_pids, _From, State) ->
  Pids = minions(State),
  {reply,{ok, Pids},State};

handle_call(process_info, _From, State) ->
  Pids = minions(State),
  Info = [erlang:process_info(Pid) || Pid <- Pids],
  {reply,{ok, Info},State};

handle_call(sing, _From, State) ->
  ok = minion_message(sing, State),
  {reply,ok,State};

handle_call(sudoku_start, _From, State) ->
  NewState = sudoku_start(State),
  {reply,ok,NewState};

handle_call(sudoku_stop, _From, State) ->
  NewState = sudoku_stop(State),
  {reply,ok,NewState};

handle_call(sudoku_stats, _From, State) ->
  Stats = sudoku_stats(State),
  {reply,Stats,State};

handle_call(minion_nodes, _From, State) ->
  Nodes = enslaved_nodes(State),
  {reply,Nodes,State};

handle_call(minion_info, _From, State) ->
  ok = minion_message(minion_info, State),
  {reply,ok,State};

handle_call(minion_crash, _From, State) ->
  ok = minion_message(crash_you_worthless_fool, State),
  {reply,havoc_caused,State};

handle_call({minion_exit, Reason}, _From, State) ->
  ok = minion_message({exit, Reason}, State),
  {reply,exit_command_given,State};

handle_call(sponge_crash, From, State) ->
  gen_server:reply(From, ouch_i_cant_look),

  %% Cause badmatch
  State = boomcrash,
  {noreply,State};

handle_call({sponge_exit, Reason}, From, State) ->
  gen_server:reply(From, byebye),

  erlang:exit(Reason),
  {noreply,State};

handle_call({send, Message}, _From, State) ->
  ok = minion_message(Message, State),
  {reply,ok,State};

handle_call(stop, _From, State) ->
  log("stop ~p~n",[_From]),
  {stop,normal,State};

handle_call(Call, From, State) ->
  log("Unexpected call ~p from ~p~n",[Call, From]),
  {noreply,State}.

handle_cast(_Msg, State) ->
  log("Unexpected cast ~p~n",[_Msg]),
  {noreply, State}.

handle_info({start_minion_supervisor, SpongeSupervisorPid}, State = #state{}) ->
  MinionSupSpec = {minion_supersup,
                   {minion_supersup, start_link, []},
                   permanent,
                   10000,
                   supervisor,
                   [minion_supersup]},
  MinionSuperSupPid = case supervisor:start_child(SpongeSupervisorPid, MinionSupSpec) of
                        {ok, Pid}                        when is_pid(Pid) -> Pid;
                        {ok, Pid, _Info}                 when is_pid(Pid) -> Pid;
                        {error, {already_started, Pid}}  when is_pid(Pid) -> Pid;
                        {error, Pid}                     when is_pid(Pid) -> Pid
                      end,
  log("The minion supersup (~p) has been attached to hypnosponge_sup (~p)~n",
      [MinionSuperSupPid, SpongeSupervisorPid]),
  
  log_children(SpongeSupervisorPid, hypnosponge_sup),
  link(MinionSuperSupPid),

  SpongePid = self(),
  _RecruiterPid = proc_lib:spawn(fun() -> minion_recruiter(SpongePid, MinionSuperSupPid) end),
  NewState = State#state{minion_supervisor=MinionSuperSupPid},
  {noreply, NewState};

handle_info({aye_dark_overlord, Minion, Node}, State = #state{}) ->
  _Ref = erlang:monitor(process, Minion),
  log("Minion ~p on ~s reporting for duty~n", [Minion, atom_to_list(Node)]),
  NewState = add_minion(Minion, Node, State),
  ok = send_sudoku(Minion, NewState),
  {noreply, NewState};

handle_info({sudoku_solved, SolvedCount, Minion, _Node}, State = #state{}) ->
  NewState = update_sudoku_count(SolvedCount, Minion, State),
  ok = send_sudoku(Minion, NewState),
  {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  Node = atom_to_list(node(Pid)),
  log("Received 'DOWN' from ~p (~s) Reason: ~p~n",[Pid, Node, _Reason]),
  
  NewState = case lists:member(Pid, minions(State)) of
               true ->
                 remove_minion(Pid, State);
               false ->
                 State
             end,
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

minion_recruiter(SpongePid, MinionSuperSupPid) ->
  log("Hello from your minion_recruiter~n",[]),
  minion_recruit_loop(SpongePid, MinionSuperSupPid).

minion_recruit_loop(SpongePid, MinionSuperSupPid) ->
  EnslavedMinionNodes = lists:sort(gen_server:call(SpongePid,minion_nodes,?DEFAULT_TIMEOUT)),
  ConnectedMinionNodes = lists:sort(connected_minion_nodes()),
  NewRecruits = ConnectedMinionNodes -- EnslavedMinionNodes,
  enslave_nodes(NewRecruits, SpongePid, MinionSuperSupPid),
  receive
    Message ->
      log("Unexpected message in minion_recruiter: ~p~n",[Message]),
      minion_recruit_loop(SpongePid, MinionSuperSupPid)
  after 1000 ->
      minion_recruit_loop(SpongePid, MinionSuperSupPid)
  end.

enslave_nodes(Nodes, SpongePid, MinionSuperSupPid) ->
  [enslave_node(Node, SpongePid, MinionSuperSupPid) || Node <- Nodes].

enslave_node(Node, SpongePid, MinionSuperSupPid) ->
  darklord_utils:load_code(Node),
  {ok, MinionSup} = supervisor:start_child(MinionSuperSupPid, [SpongePid, Node]),

  log("Enslaved node ~p (remote minion supervisor is ~p)~n",[Node, MinionSup]),
  log_children(MinionSuperSupPid, minion_supersup),
  MinionSup.

connected_minion_nodes() ->
  nodes() -- overlord_nodes().

overlord_nodes() ->
  case application:get_env(kernel, sync_nodes_mandatory) of
    {ok, Nodes} ->
      Nodes;
    _Else ->
      [node()]
  end.


sudoku_start(State) ->
  NewState = State#state{sudoku_started=true},
  [send_sudoku(Minion, NewState) || Minion <- minions(NewState)],
  NewState.

sudoku_stop(State) ->
  NewState = State#state{sudoku_started=false},
  NewState.

sudoku_stats(#state{sudoku_stats=Stats}) ->
  dict:to_list(Stats).

update_sudoku_count(SolvedCount, Minion, State = #state{sudoku_stats=Stats}) ->
  Node = node(Minion),
  NewStats = dict:update_counter(Node, SolvedCount, Stats),
  State#state{sudoku_stats=NewStats}.

send_sudoku(Minion, #state{sudoku_started=true}) ->
  Filename = filename:join(code:priv_dir(sudoku), "top95.txt"),
  {ok, BinString} = file:read_file(Filename),
  Minion ! {sudoku, BinString},
  ok;
send_sudoku(_Minion, #state{}) ->
  ok.

minions(#state{minions=Minions}) ->
  Minions.

enslaved_nodes(State) ->
  [node(Pid) || Pid <- minions(State)].

add_minion(Minion, _Node, State) ->
  Minions = [Minion|minions(State)],
  State#state{minions=Minions}.

remove_minion(Minion, State) ->
  Minions = minions(State) -- [Minion],
  State#state{minions=Minions}.

minion_message(Message, State) ->
  MinionPids = minions(State),
  MessageFun = fun(Pid) ->
                   Pid ! Message
               end,
  lists:foreach(MessageFun, MinionPids).

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).

log_children(SupPid, SupName) ->
  Children = [Pid || {_,Pid,_WorkerOrSupervisor,_Name} <- supervisor:which_children(SupPid)],
  log("~p (~p) now has ~p children: ~p~n",
      [SupName, SupPid, length(Children), Children]).
