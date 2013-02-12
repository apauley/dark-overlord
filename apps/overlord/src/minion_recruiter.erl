%%%-------------------------------------------------------------------
%%% @author Andreas Pauley <>
%%% Created : 12 Feb 2013 by Andreas Pauley <>
%%%-------------------------------------------------------------------
-module(minion_recruiter).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(DEFAULT_TIMEOUT, 5000).

-record(state, {sponge,
                minion_supersup}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(SpongePid, MinionSuperSupPid) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [SpongePid, MinionSuperSupPid], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SpongePid, MinionSuperSupPid]) ->
  log("Hello from your minion_recruiter~n",[]),
  self() ! recruit_minions,
  {ok, #state{sponge=SpongePid, minion_supersup=MinionSuperSupPid}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(recruit_minions, State=#state{sponge=Sponge, minion_supersup=MinionSuperSup}) ->
  _MinionSupervisors = recruit_minions(Sponge, MinionSuperSup),
  timer:send_after(1000, recruit_minions),
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

recruit_minions(SpongePid, MinionSuperSupPid) ->
  UnsortedMinionNodes = gen_server:call(SpongePid,minion_nodes,?DEFAULT_TIMEOUT),
  EnslavedMinionNodes = lists:sort(UnsortedMinionNodes),
  ConnectedMinionNodes = lists:sort(connected_minion_nodes()),
  NewRecruits = ConnectedMinionNodes -- EnslavedMinionNodes,
  MinionSuperVisors = enslave_nodes(NewRecruits, SpongePid, MinionSuperSupPid),
  MinionSuperVisors.

enslave_nodes(Nodes, SpongePid, MinionSuperSupPid) ->
  [enslave_node(Node, SpongePid, MinionSuperSupPid) || Node <- Nodes].

enslave_node(Node, SpongePid, MinionSuperSupPid) ->
  darklord_utils:load_code(Node),
  {ok, MinionSup} = supervisor:start_child(MinionSuperSupPid, [SpongePid, Node]),

  log("Enslaved node ~p (remote minion supervisor is ~p)~n",[Node, MinionSup]),
  darklord_utils:log_supervisor_children(?MODULE, MinionSuperSupPid, minion_supersup),
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

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
