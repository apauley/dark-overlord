%%% @author Andreas Pauley <>
%%% Created :  8 Feb 2013 by Andreas Pauley <>

-module(minion_makeshift_sup).

-export([start_link/2,
         init/1]).

-export([ping_server/1]).

-record(state, {minion,
                ping_server,
                sponge_pid,
                sponge_node}).

start_link(HypnoSpongePid, RemoteNode) ->
  Pid = proc_lib:spawn_link(RemoteNode, ?MODULE, init, [{HypnoSpongePid, node()}]),
  {ok, Pid}.

init({HypnoSpongePid, HypnoSpongeNode}) ->
  process_flag(trap_exit, true),
  log("init/1 minion for overlord who lives at ~s (~p)~n", [atom_to_list(HypnoSpongeNode), HypnoSpongePid]),

  State = start_ping_server(#state{sponge_pid=HypnoSpongePid,
                                   sponge_node=HypnoSpongeNode}),

  NewState = start_minion(State),
  supervisor_wait(NewState).

supervisor_wait(State) ->
  receive
    Message ->
      handle_message(Message, State)
  after 5 ->
      supervisor_wait(State)
  end.

handle_message(_Message={exit, Reason}, State) ->
  log("handle_message unknown exit received. Reason: ~p~n", [Reason]),
  NewState = start_minion(State),
  supervisor_wait(NewState);
handle_message(Message, State) ->
  log("handle_message unknown message received: ~p~n", [Message]),
  supervisor_wait(State).

start_minion(State = #state{sponge_pid=HypnoSpongePid, sponge_node=HypnoSpongeNode}) ->
  {ok, Minion} = minion:start_link(HypnoSpongePid, HypnoSpongeNode),
  State#state{minion=Minion}.

start_ping_server(State = #state{sponge_node=HypnoSpongeNode}) ->
  PingPid = proc_lib:spawn_link(?MODULE, ping_server, [HypnoSpongeNode]),
  State#state{ping_server=PingPid}.

ping_server(Node) ->
  log("ping_server started that pings ~p~n", [Node]),
  ping_loop(Node).

ping_loop(Node) ->
  _PingPang = net_adm:ping(Node),
  timer:sleep(1000),
  ping_loop(Node).

log(String, Params) ->
  darklord_utils:log(?MODULE, String, Params).
